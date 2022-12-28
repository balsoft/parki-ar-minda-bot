{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

module Bot (bot) where

import Control.Monad (forM, forM_, unless, void, when)
import Control.Monad.Except (MonadError (catchError))
import Control.Monad.IO.Unlift (MonadIO (liftIO))
import Control.Monad.Reader (ReaderT)
import qualified Data.List as L
import Data.Maybe (catMaybes, mapMaybe, maybeToList)
import Data.Text as T (Text, intercalate, isPrefixOf, pack, replace, unpack)
import Data.Text.IO (hPutStrLn)
import Data.Time
import Database.Persist
import Database.Persist.Sqlite (ConnectionPool, SqlBackend)
import I18N
import Persist
import Servant.Client hiding (Response)
import System.IO (stderr)
import Telegram.Bot.API
import Telegram.Bot.Monadic
import Text.Shakespeare.I18N (Lang)
import Util

-- | Generate a grid of buttons for choosing a time
timeGrid ::
  Maybe Text ->
  -- | Don't show times earlier than this
  Maybe Text ->
  -- | Mark this time as chosen
  [[InlineKeyboardButton]]
timeGrid s p =
  fmap reverse $
    reverse $
      chunksOf 2 $
        reverse
          (fmap (\t -> ikb (checkBox (showHourMinutes t)) (showHourMinutes t)) times)
  where
    (timeLessThan, timeNoMoreThan) =
      case parseHourMinutesM =<< s of
        Just (TimeOfDay sHour sMinute _) ->
          ( (TimeOfDay (sHour + 1) sMinute 0 >),
            (TimeOfDay (sHour + 3) sMinute 0 >=)
          )
        Nothing -> (const False, const True)
    times =
      takeWhile timeNoMoreThan $
        dropWhile
          timeLessThan
          [TimeOfDay hour minute 0 | hour <- [9 .. 21], minute <- [0, 30]]
    checkBox t = checkIf (p == Just t) <> t

-- | Send a message which represents a step in menu
menuStepMessage ::
  [Lang] ->
  ChatChannel ->
  BotMessage ->
  [[InlineKeyboardButton]] ->
  ClientM MessageId
menuStepMessage langs ChatChannel {..} msg grid =
  messageMessageId . responseResult
    <$> sendMessage
      (sendMessageRequest channelChatId (tr langs msg))
        { sendMessageReplyMarkup = Just $ ik grid,
          sendMessageParseMode = Just MarkdownV2
        }

garageStep ::
  [Lang] ->
  ConnectionPool ->
  ChatChannel ->
  VolunteerId ->
  ClientM (Either CallbackQuery ())
garageStep langs pool chat@ChatChannel {..} volunteer = do
  garages <- runInPool pool (selectList [] [])
  let grid c =
        [ [ikb (checkIf (c == Just gid) <> garageName) (showSqlKey gid)]
          | Entity gid (Garage {..}) <- garages
        ]
  msg <- menuStepMessage langs chat MsgChooseGarage (grid Nothing)
  query <- getCallbackQueryWithData channelUpdateChannel
  iterateUntilRight
    query
    ( \q ->
        case q of
          CallbackQuery
            { callbackQueryData = Just garage,
              callbackQueryMessage = Just Message {messageMessageId}
            }
              | messageMessageId == msg -> do
                  let key = readSqlKey garage
                  void
                    ( editMessageReplyMarkup
                        (editMessageReplyMarkupRequest (Just $ ik $ grid (Just key)))
                          { editMessageReplyMarkupChatId =
                              Just $ SomeChatId channelChatId,
                            editMessageReplyMarkupMessageId = Just msg
                          }
                    )
                    `catchError` const (pure ())
                  fmap Right <$> dayStep langs pool chat volunteer key
          _ -> pure $ Right $ Left q
    )
    <* deleteMessage channelChatId msg

dayStep ::
  [Lang] ->
  ConnectionPool ->
  ChatChannel ->
  VolunteerId ->
  GarageId ->
  ClientM (Either CallbackQuery ())
dayStep langs pool chat@ChatChannel {..} volunteer garage = do
  now <- localDay . zonedTimeToLocalTime <$> liftIO getZonedTime
  days <-
    runInPool pool $
      selectList
        [OpenDayGarage ==. garage, OpenDayDate >. now, OpenDayAvailable ==. True]
        []
  if null days
    then do
      msg <- sendMessage (sendMessageRequest channelChatId (forbidden <> tr langs MsgNoDays))
      (Left <$> getCallbackQueryWithData channelUpdateChannel)
        <* deleteMessage channelChatId (messageMessageId $ responseResult msg)
    else do
      let grid p =
            [ [ ikb
                  (checkIf (p == Just did) <> showDay langs openDayDate)
                  (showSqlKey did)
              ]
              | Entity did (OpenDay {..}) <- days
            ]
      msg <- menuStepMessage langs chat MsgChooseDay (grid Nothing)
      query <- getCallbackQueryWithData channelUpdateChannel
      iterateUntilRight
        query
        ( \q ->
            case q of
              CallbackQuery
                { callbackQueryData = Just day,
                  callbackQueryMessage = Just Message {messageMessageId}
                }
                  | messageMessageId == msg -> do
                      let key = readSqlKey day
                      void
                        ( editMessageReplyMarkup
                            ( editMessageReplyMarkupRequest
                                (Just $ ik $ grid (Just key))
                            )
                              { editMessageReplyMarkupChatId =
                                  Just $ SomeChatId channelChatId,
                                editMessageReplyMarkupMessageId = Just msg
                              }
                        )
                        `catchError` const (pure ())
                      fmap Right <$> existingSlotsStep langs pool chat volunteer key
              _ -> pure $ Right $ Left q
        )
        <* deleteMessage channelChatId msg

existingSlotsStep ::
  [Lang] ->
  ConnectionPool ->
  ChatChannel ->
  VolunteerId ->
  OpenDayId ->
  ClientM (Either CallbackQuery ())
existingSlotsStep langs pool chat@ChatChannel {..} volunteer day = do
  Just OpenDay {openDayAvailable} <- runInPool pool $ get day
  if not openDayAvailable
    then do
      msg' <- sendMessage (sendMessageRequest channelChatId (forbidden <> tr langs MsgDayUnavailable))
      Left
        <$> getCallbackQueryWithData channelUpdateChannel
        <* deleteMessage channelChatId (messageMessageId $ responseResult msg')
    else do
      slots <-
        runInPool pool $
          selectList
            [ScheduledSlotDay ==. day, ScheduledSlotUser !=. volunteer]
            []
      if null slots
        then startTimeStep langs pool chat volunteer day
        else do
          let grid p =
                [ [ ikb
                      ( checkIf (p == Just (Right sid))
                          <> showHourMinutes scheduledSlotStartTime
                          <> "-"
                          <> showHourMinutes scheduledSlotEndTime
                      )
                      (showSqlKey sid)
                  ]
                  | Entity sid ScheduledSlot {..} <- slots
                ]
                  ++ [ [ ikb
                           ( checkIf (p == Just (Left "new"))
                               <> tr langs MsgNewAppointment
                           )
                           "new"
                       ]
                     ]
          msg <- menuStepMessage langs chat MsgChooseExistingSlot (grid Nothing)
          query <- getCallbackQueryWithData channelUpdateChannel
          iterateUntilRight
            query
            ( \q ->
                case q of
                  CallbackQuery
                    { callbackQueryData = Just slot,
                      callbackQueryMessage = Just Message {messageMessageId}
                    }
                      | messageMessageId == msg -> do
                          fmap Right
                            <$> if slot == "new"
                              then do
                                void
                                  ( editMessageReplyMarkup
                                      ( editMessageReplyMarkupRequest
                                          (Just $ ik $ grid (Just $ Left "new"))
                                      )
                                        { editMessageReplyMarkupChatId =
                                            Just $ SomeChatId channelChatId,
                                          editMessageReplyMarkupMessageId = Just msg
                                        }
                                  )
                                  `catchError` const (pure ())
                                startTimeStep langs pool chat volunteer day
                              else do
                                let key = readSqlKey slot
                                void
                                  ( editMessageReplyMarkup
                                      ( editMessageReplyMarkupRequest
                                          (Just $ ik $ grid (Just $ Right key))
                                      )
                                        { editMessageReplyMarkupChatId =
                                            Just $ SomeChatId channelChatId,
                                          editMessageReplyMarkupMessageId = Just msg
                                        }
                                  )
                                  `catchError` const (pure ())
                                Just slot' <- runInPool pool $ get (readSqlKey slot)
                                confirmStep
                                  langs
                                  pool
                                  chat
                                  slot'
                                    { scheduledSlotUser = volunteer,
                                      scheduledSlotConfirmed = Nothing,
                                      scheduledSlotReminderSent = False
                                    }
                  _ -> pure $ Right $ Left q
            )
            <* deleteMessage channelChatId msg

startTimeStep ::
  [Lang] ->
  ConnectionPool ->
  ChatChannel ->
  VolunteerId ->
  OpenDayId ->
  ClientM (Either CallbackQuery ())
startTimeStep langs pool chat@ChatChannel {..} volunteer day = do
  msg <-
    menuStepMessage langs chat MsgChooseStartTime (timeGrid Nothing Nothing)
  query <- getCallbackQueryWithData channelUpdateChannel
  iterateUntilRight
    query
    ( \q ->
        case q of
          CallbackQuery
            { callbackQueryData = Just time,
              callbackQueryMessage = Just Message {messageMessageId}
            }
              | messageMessageId == msg -> do
                  void
                    ( editMessageReplyMarkup
                        ( editMessageReplyMarkupRequest
                            (Just $ ik $ timeGrid Nothing (Just time))
                        )
                          { editMessageReplyMarkupChatId =
                              Just $ SomeChatId channelChatId,
                            editMessageReplyMarkupMessageId = Just msg
                          }
                    )
                    `catchError` const (pure ())
                  fmap Right <$> endTimeStep langs pool chat volunteer day time
          _ -> pure $ Right $ Left q
    )
    <* deleteMessage channelChatId msg

endTimeStep ::
  [Lang] ->
  ConnectionPool ->
  ChatChannel ->
  VolunteerId ->
  OpenDayId ->
  Text ->
  ClientM (Either CallbackQuery ())
endTimeStep langs pool chat@ChatChannel {..} volunteer day startTime = do
  msg <-
    menuStepMessage
      langs
      chat
      MsgChooseEndTime
      (timeGrid (Just startTime) Nothing)
  query <- getCallbackQueryWithData channelUpdateChannel
  iterateUntilRight
    query
    ( \q ->
        case q of
          CallbackQuery
            { callbackQueryData = Just endTime,
              callbackQueryMessage = Just Message {messageMessageId}
            }
              | messageMessageId == msg -> do
                  void
                    ( editMessageReplyMarkup
                        ( editMessageReplyMarkupRequest
                            (Just $ ik $ timeGrid (Just startTime) (Just endTime))
                        )
                          { editMessageReplyMarkupChatId =
                              Just $ SomeChatId channelChatId,
                            editMessageReplyMarkupMessageId = Just msg
                          }
                    )
                    `catchError` const (pure ())
                  scheduledSlotStartTime <- parseHourMinutesM startTime
                  scheduledSlotEndTime <- parseHourMinutesM endTime
                  fmap Right
                    <$> confirmStep
                      langs
                      pool
                      chat
                      ScheduledSlot
                        { scheduledSlotDay = day,
                          scheduledSlotStartTime,
                          scheduledSlotEndTime,
                          scheduledSlotUser = volunteer,
                          scheduledSlotConfirmed = Nothing,
                          scheduledSlotReminderSent = False
                        }
          _ -> pure $ Right $ Left q
    )
    <* deleteMessage channelChatId msg

confirmStep ::
  [Lang] ->
  ConnectionPool ->
  ChatChannel ->
  ScheduledSlot ->
  ClientM (Either CallbackQuery ())
confirmStep langs pool chat@ChatChannel {..} slot@ScheduledSlot {..} = do
  otherDuties <-
    runInPool pool $
      mapM (getSlotDesc langs . entityVal)
        =<< selectList
          ( [ScheduledSlotDay ==. scheduledSlotDay, ScheduledSlotStartTime <=. scheduledSlotStartTime, ScheduledSlotEndTime >=. scheduledSlotStartTime]
              ||. [ScheduledSlotDay ==. scheduledSlotDay, ScheduledSlotEndTime >=. scheduledSlotEndTime, ScheduledSlotStartTime <=. scheduledSlotEndTime]
              ||. [ScheduledSlotDay ==. scheduledSlotDay, ScheduledSlotStartTime <=. scheduledSlotStartTime, ScheduledSlotEndTime >=. scheduledSlotEndTime]
              ||. [ScheduledSlotDay ==. scheduledSlotDay, ScheduledSlotStartTime >=. scheduledSlotStartTime, ScheduledSlotEndTime <=. scheduledSlotEndTime]
          )
          []
  if not $ null otherDuties
    then do
      void $
        sendMessage
          (sendMessageRequest channelChatId (tr langs MsgOtherDuties <> "\n\n" <> intercalate "\n\n" otherDuties))
            { sendMessageParseMode = Just MarkdownV2
            }
      Left <$> getCallbackQueryWithData channelUpdateChannel
    else do
      slotDesc <- runInPool pool $ getSlotDesc langs slot
      msg <-
        messageMessageId . responseResult
          <$> sendMessage
            ( sendMessageRequest
                channelChatId
                (tr langs MsgConfirm <> "\n" <> slotDesc)
            )
              { sendMessageReplyMarkup =
                  Just $
                    SomeInlineKeyboardMarkup $
                      InlineKeyboardMarkup [[ikb (allGood <> tr langs MsgYesConfirm) "yes"]],
                sendMessageParseMode = Just MarkdownV2
              }
      query <-
        ( getCallbackQueryWithData channelUpdateChannel
            <* deleteMessage channelChatId msg
          )
          >>= \case
            CallbackQuery
              { callbackQueryData = Just "yes",
                callbackQueryMessage = Just Message {messageMessageId = mid}
              }
                | mid == msg -> do
                    Just OpenDay {..} <- runInPool pool $ get scheduledSlotDay
                    if not openDayAvailable
                      then do
                        msg' <- sendMessage (sendMessageRequest channelChatId (forbidden <> tr langs MsgDayUnavailable))
                        getCallbackQueryWithData channelUpdateChannel
                          <* deleteMessage
                            channelChatId
                            (messageMessageId $ responseResult msg')
                      else do
                        slotId <- runInPool pool $ insert slot
                        slotConfirmedStep langs pool chat slot slotId
            q -> pure q
      pure $ Left query

cancelButton :: [Lang] -> ScheduledSlotId -> InlineKeyboardButton
cancelButton langs slotId = ikb (tr langs MsgCantCome) ("cancel_" <> showSqlKey slotId)

slotConfirmedStep ::
  [Lang] ->
  ConnectionPool ->
  ChatChannel ->
  ScheduledSlot ->
  ScheduledSlotId ->
  ClientM CallbackQuery
slotConfirmedStep langs pool chat@ChatChannel {..} slot slotId = do
  slotDesc <- runInPool pool $ getSlotDesc langs slot
  msg' <-
    messageMessageId . responseResult
      <$> sendMessage
        ( sendMessageRequest
            channelChatId
            (allGood <> tr langs MsgConfirmed <> "\n" <> slotDesc)
        )
          { sendMessageReplyMarkup =
              Just $
                ik [[cancelButton langs slotId], [ikb (tr langs MsgDone) "done"]],
            sendMessageParseMode = Just MarkdownV2
          }
  ignoreUntilRight
    ( getCallbackQueryWithData channelUpdateChannel >>= \case
        CallbackQuery {callbackQueryData = Just d} | d == ("cancel_" <> showSqlKey slotId) -> do
          cancelled <- askCancelSlot langs pool chat msg' slotId
          if cancelled then Right <$> getCallbackQueryWithData channelUpdateChannel else pure (Left ())
        q -> do
          void $
            editMessageText
              (editMessageTextRequest (allGood <> tr langs MsgConfirmedShort <> "\n" <> slotDesc))
                { editMessageTextChatId =
                    Just $ SomeChatId channelChatId,
                  editMessageTextParseMode = Just MarkdownV2,
                  editMessageTextMessageId = Just msg',
                  editMessageTextReplyMarkup = Just $ ik [[cancelButton langs slotId]]
                }
          pure (Right q)
    )

signUp :: [Lang] -> ChatChannel -> VolunteerId -> ConnectionPool -> ClientM ()
signUp langs chat@ChatChannel {channelChatId = channelChatId@(ChatId _cid)} volunteer pool = do
  introMsg <- messageMessageId . responseResult <$> introCancel
  void $ garageStep langs pool chat volunteer
  void $ deleteMessage channelChatId introMsg
  where
    introCancel =
      sendMessage
        (sendMessageRequest channelChatId (tr langs MsgSignup))
          { sendMessageReplyMarkup =
              Just $ ik [[ikb (tr langs MsgCancel) "cancel"]],
            sendMessageParseMode = Just MarkdownV2
          }

list :: [Lang] -> ChatChannel -> VolunteerId -> ConnectionPool -> ClientM ()
list langs ChatChannel {channelChatId} volunteer pool = do
  today <- localDay . zonedTimeToLocalTime <$> liftIO getZonedTime
  slots <-
    runInPool pool $ do
      days <- fmap entityKey <$> selectList [OpenDayDate >=. today] []
      selectList
        [ScheduledSlotUser ==. volunteer, ScheduledSlotDay <-. days]
        []
  when (null slots) $ void $ send channelChatId langs MsgNoSlots
  forM_ slots $ \(Entity slotId slot) -> do
    slotDesc <- runInPool pool $ getSlotDesc langs slot
    sendMessage
      ( sendMessageRequest channelChatId slotDesc
      )
        { sendMessageParseMode = Just MarkdownV2,
          sendMessageReplyMarkup = Just $ ik [[cancelButton langs slotId]]
        }

subscribe ::
  [Lang] -> ChatChannel -> TelegramUserId -> ConnectionPool -> ClientM ()
subscribe langs ChatChannel {channelChatId} uid pool = do
  void $ runInPool pool $ upsert (Subscription uid) []
  void $ send channelChatId langs MsgSubscribed

unsubscribe ::
  [Lang] -> ChatChannel -> TelegramUserId -> ConnectionPool -> ClientM ()
unsubscribe langs ChatChannel {channelChatId} uid pool = do
  void $ runInPool pool $ deleteBy $ UniqueSubscription uid
  void $ send channelChatId langs MsgUnsubscribed

deleteUser :: ConnectionPool -> VolunteerId -> ClientM ()
deleteUser pool vid = do
  slots <- runInPool pool $ selectList [ScheduledSlotUser ==. vid] []
  forM_ slots $ \(Entity slotId _) -> cancelSlot pool slotId
  runInPool pool $ do
    Just Volunteer {..} <- get vid
    deleteBy $ UniqueSubscription volunteerUser
    delete vid
    delete volunteerUser

askDeleteUser ::
  [Lang] ->
  ChatChannel ->
  VolunteerId ->
  ConnectionPool ->
  ClientM ()
askDeleteUser langs chat@ChatChannel {channelChatId} volunteer pool = do
  void $ sendMessage (sendMessageRequest channelChatId (attention <> tr langs MsgAreYouSure))
  untilRight (getNewMessage chat) (const $ pure ()) >>= \case
    Message {messageText = Just txt}
      | txt == tr langs MsgIAmSure -> do
          deleteUser pool volunteer
          void $ send channelChatId langs MsgDeleted
    _ -> void $ send channelChatId langs MsgNotDeleting

cancelSlot :: ConnectionPool -> ScheduledSlotId -> ClientM ()
cancelSlot pool slotId = do
  (slotFullDesc, slotDesc, dayAvailable, langs, TelegramUser {..}) <-
    runInPool pool $ do
      Just slot <- get slotId
      Just Volunteer {..} <- get $ scheduledSlotUser slot
      Just OpenDay {..} <- get $ scheduledSlotDay slot
      Just user@TelegramUser {..} <- get volunteerUser
      let langs = maybeToList telegramUserLang
      fullDesc <- getSlotFullDesc langs slot
      desc <- getSlotDesc langs slot
      pure (fullDesc, desc, openDayAvailable, langs, user)
  runInPool pool $ delete slotId
  void $
    sendMessage
      ( sendMessageRequest
          (ChatId (fromIntegral telegramUserUserId))
          (attention <> tr langs MsgYourSlotCancelled <> "\n" <> slotDesc)
      )
        { sendMessageParseMode = Just MarkdownV2
        }
  admins <- runInPool pool getAdmins
  unless dayAvailable $
    forM_ admins $ \(TelegramUser auid lang _ _) -> do
      let langs = maybeToList lang
      void $
        sendMessage
          ( sendMessageRequest
              (ChatId (fromIntegral auid))
              (attention <> tr langs MsgSlotCancelled <> "\n" <> slotFullDesc)
          )
            { sendMessageParseMode = Just MarkdownV2,
              sendMessageReplyMarkup =
                Just (ik [[ikb (tr langs MsgLock) "lock"]])
            }

askCancelSlot ::
  [Lang] ->
  ConnectionPool ->
  ChatChannel ->
  MessageId ->
  ScheduledSlotId ->
  ClientM Bool
askCancelSlot langs pool ChatChannel {..} originalMsgId slotId = do
  Just slot <- runInPool pool $ get slotId
  slotDesc <- runInPool pool $ getSlotDesc langs slot
  msg <-
    responseResult
      <$> sendMessage
        ( sendMessageRequest
            channelChatId
            (attention <> tr langs MsgSureCancel <> "\n" <> slotDesc)
        )
          { sendMessageParseMode = Just MarkdownV2,
            sendMessageReplyMarkup =
              Just $
                ik
                  [ [ ikb (tr langs MsgYesCancel) "cancel",
                      ikb (tr langs MsgNoIWillCome) "will_come"
                    ]
                  ]
          }
  doCancel <-
    ignoreUntilRight
      ( getUpdate channelUpdateChannel >>= \case
          SomeNewCallbackQuery
            q@CallbackQuery
              { callbackQueryData = Just txt,
                callbackQueryMessage = Just msg'
              }
              | messageMessageId msg' == messageMessageId msg -> do
                  void $
                    answerCallbackQuery
                      (answerCallbackQueryRequest (callbackQueryId q))
                  pure $
                    case txt of
                      "will_come" -> Right False
                      "cancel" -> Right True
                      _ -> Left ()
          _ -> pure (Left ())
      )
  void $ deleteMessage channelChatId (messageMessageId msg)
  when doCancel $ do
    void $ deleteMessage channelChatId originalMsgId
    cancelSlot pool slotId
  pure doCancel

confirmSlot ::
  [Lang] ->
  ConnectionPool ->
  ChatChannel ->
  MessageId ->
  ScheduledSlotId ->
  ClientM ()
confirmSlot langs pool ChatChannel {..} originalMsgId slotId = do
  Just slot <- runInPool pool $ get slotId
  slotDesc <- runInPool pool $ getSlotDesc langs slot
  void $ deleteMessage channelChatId originalMsgId
  runInPool pool $ update slotId [ScheduledSlotConfirmed =. Just True]
  void $
    sendMessage
      ( sendMessageRequest
          channelChatId
          (allGood <> tr langs MsgConfirmedShort <> "\n" <> slotDesc)
      )
        { sendMessageParseMode = Just MarkdownV2,
          sendMessageReplyMarkup =
            Just $
              ik [[cancelButton langs slotId]]
        }

askForPermission :: ConnectionPool -> User -> ClientM ()
askForPermission pool User {userId = UserId uid} = do
  admins <- runInPool pool getAdmins
  Just (Entity tuid user) <-
    runInPool pool (getBy $ UniqueTelegramUser (fromIntegral uid))
  forM_ admins $ \(TelegramUser auid lang _ _) -> do
    let langs = maybeToList lang
    void $
      sendMessage
        ( sendMessageRequest
            (ChatId (fromIntegral auid))
            (tr langs $ MsgVolunteerRequest $ renderUser user)
        )
          { sendMessageParseMode = Just MarkdownV2,
            sendMessageReplyMarkup =
              Just $
                ik
                  [ [ ikb (allGood <> tr langs MsgAllow) ("allow_" <> showSqlKey tuid),
                      ikb (bad <> tr langs MsgDecline) ("decline_" <> showSqlKey tuid)
                    ]
                  ]
          }

allowVolunteer :: ConnectionPool -> TelegramUserId -> ClientM ()
allowVolunteer pool tuid = do
  (admins, Just user@TelegramUser {telegramUserLang, telegramUserUserId}) <-
    runInPool pool $ do
      void $ upsert Volunteer {volunteerUser = tuid} []
      admins <- getAdmins
      user <- get tuid
      pure (admins, user)
  forM_ admins $ \(TelegramUser auid lang _ _) -> do
    let langs = maybeToList lang
    void $
      sendMessage
        ( sendMessageRequest
            (ChatId (fromIntegral auid))
            (tr langs $ MsgNewVolunteer $ renderUser user)
        )
          { sendMessageParseMode = Just MarkdownV2,
            sendMessageReplyMarkup =
              Just $ ik [[ikb (forbidden <> tr langs MsgBan) ("ban_" <> showSqlKey tuid)]]
          }
  void $
    sendMessage
      ( sendMessageRequest
          (ChatId (fromIntegral telegramUserUserId))
          (tr (maybeToList telegramUserLang) MsgVolunteer)
      )
        { sendMessageParseMode = Just MarkdownV2
        }

banVolunteer :: ConnectionPool -> TelegramUserId -> ClientM ()
banVolunteer pool tuid = do
  (admins, user, volunteer) <-
    runInPool pool $ do
      Just user <- get tuid
      Just volunteer <- getBy $ UniqueVolunteer tuid
      admins <- getAdmins
      pure (admins, user, volunteer)
  deleteUser pool (entityKey volunteer)
  forM_ admins $ \(TelegramUser auid lang _ _) -> do
    let langs = maybeToList lang
    sendMessage
      ( sendMessageRequest
          (ChatId (fromIntegral auid))
          (tr langs $ MsgVolunteerRemoved $ renderUser user)
      )
        { sendMessageParseMode = Just MarkdownV2
        }

checkbox :: Bool -> Text
checkbox False = "☐"
checkbox True = "☑"

inlinePollMarkup ::
  Foldable t => Bool -> [[(Text, Text)]] -> t Text -> SomeReplyMarkup
inlinePollMarkup doneable grid current =
  ik $
    fmap (fmap (\(t, d) -> ikb (checkbox (d `elem` current) <> " " <> t) d)) grid
      ++ [[ikb "Done" "done"] | doneable]
      ++ [[ikb "Cancel" "cancel"]]

inlinePoll ::
  ChatChannel ->
  ([Text] -> Bool) ->
  Text ->
  [[(Text, Text)]] ->
  [Text] ->
  ClientM (Maybe [Text])
inlinePoll ChatChannel {..} acceptable question grid initial = do
  Response {responseResult = msg} <-
    sendMessage $
      (sendMessageRequest channelChatId question)
        { sendMessageReplyMarkup =
            Just $ inlinePollMarkup (acceptable initial) grid initial
        }
  let getPollResult lst = do
        q <-
          ignoreUntilRight
            ( getUpdate channelUpdateChannel >>= \case
                SomeNewCallbackQuery c@CallbackQuery {callbackQueryMessage = Just msg'}
                  | messageMessageId msg' == messageMessageId msg ->
                      pure $ Right c
                _ -> pure $ Left ()
            )
        let a =
              answerCallbackQuery $
                answerCallbackQueryRequest $
                  callbackQueryId q
        case callbackQueryData q of
          Just "done"
            | acceptable lst -> a >> pure (Just lst)
          Just "cancel" -> pure Nothing
          Just d
            | any (any ((d ==) . snd)) grid -> do
                let lst' =
                      if d `elem` lst
                        then L.delete d lst
                        else d : lst
                void
                  ( editMessageReplyMarkup
                      ( editMessageReplyMarkupRequest
                          (Just $ inlinePollMarkup (acceptable lst') grid lst')
                      )
                        { editMessageReplyMarkupMessageId =
                            Just $ messageMessageId msg,
                          editMessageReplyMarkupChatId =
                            Just $ SomeChatId channelChatId
                        }
                  )
                  `catchError` const (pure ())
                _ <- a
                getPollResult lst'
          _ -> a >> getPollResult lst
  getPollResult initial

getSubscriptions :: MonadIO m => ReaderT SqlBackend m [TelegramUser]
getSubscriptions = do
  admins <- selectList [] []
  subscriptions <- selectList [] []
  let ids =
        fmap (adminUser . entityVal) admins
          ++ fmap (subscriptionUser . entityVal) subscriptions
  catMaybes <$> mapM get ids

makeSchedule :: [Lang] -> ConnectionPool -> ChatChannel -> ClientM ()
makeSchedule langs pool chat = do
  garages <- runInPool pool (selectList [] [])
  now <- localDay . zonedTimeToLocalTime <$> liftIO getZonedTime
  let nextWeek =
        Prelude.take 7 $ dropWhile ((/= Monday) . dayOfWeek) $ drop 1 [now ..]
  forM_ garages $ \(Entity gid Garage {..}) -> do
    defaultDays <-
      fmap (defaultOpenDayDayOfWeek . entityVal)
        <$> runInPool pool (selectList [DefaultOpenDayGarage ==. gid] [])
    let defaultDays' =
          pack . showGregorian
            <$> Prelude.filter ((`elem` defaultDays) . dayOfWeek) nextWeek
    inlinePoll
      chat
      (const True)
      (tr langs (MsgGarageOpenDays garageName))
      [[(showDay langs d, pack $ showGregorian d)] | d <- nextWeek]
      defaultDays'
      >>= ( \case
              Nothing -> pure ()
              Just days -> do
                subs <-
                  runInPool pool $ do
                    deleteWhere [DefaultOpenDayGarage ==. gid]
                    mapM_ (insert . DefaultOpenDay gid) (fmap dayOfWeek days)
                    deleteWhere [OpenDayGarage ==. gid, OpenDayDate /<-. days]
                    mapM_
                      ( \day ->
                          upsertBy
                            (UniqueDay day gid)
                            (OpenDay gid day True)
                            [OpenDayAvailable =. True]
                      )
                      days
                    getSubscriptions
                forM_ subs $ \(TelegramUser uid lang _ _) -> do
                  catchError
                    ( void $
                        sendMessage
                          ( sendMessageRequest
                              (ChatId (fromIntegral uid))
                              (news <> tr (maybeToList lang) (MsgNewScheduleFor garageName))
                          )
                            { sendMessageParseMode = Just MarkdownV2,
                              sendMessageReplyMarkup =
                                Just $
                                  ik
                                    [ [ ikb
                                          (showDay langs date)
                                          ("signup" <> pack (show (garageName, date)))
                                      ]
                                      | date <- days
                                    ]
                            }
                    )
                    (liftIO . print)
          )
        . fmap (L.sort . mapMaybe parseGregorian)

renderSchedule :: [Lang] -> [(Day, [(TimeOfDay, TimeOfDay)])] -> Text
renderSchedule langs s =
  intercalate
    "\n"
    [ escapeMd (showDay langs day)
        <> "\n\n"
        <> intercalate
          "\n"
          ( fmap
              ( \(start, end) ->
                  tr
                    langs
                    (MsgScheduleTime (showHourMinutes start) (showHourMinutes end))
              )
              times
          )
      | (day, times) <- L.sort s,
        times /= []
    ]

lockSchedule :: [Lang] -> ConnectionPool -> ChatChannel -> ClientM ()
lockSchedule langs pool ChatChannel {..} = do
  void $
    sendMessage
      (sendMessageRequest channelChatId (tr langs MsgLocked))
        { sendMessageParseMode = Just MarkdownV2,
          sendMessageReplyMarkup =
            Just $ ik [[ikb (tr langs MsgUnlock) "unlock"]]
        }
  garages <- runInPool pool (selectList [] [])
  now <- localDay . zonedTimeToLocalTime <$> liftIO getZonedTime
  let nextWeek =
        Prelude.take 7 $ dropWhile ((/= Monday) . dayOfWeek) $ drop 1 [now ..]
  forM_ garages $ \(Entity gid Garage {..}) -> do
    let selector = [OpenDayGarage ==. gid, OpenDayDate <-. nextWeek]
    openDays <-
      runInPool pool $ do
        updateWhere selector [OpenDayAvailable =. False]
        selectList selector []
    openDaysWithSlots <-
      forM openDays $ \(Entity day OpenDay {..}) -> do
        slots <- runInPool pool $ selectList [ScheduledSlotDay ==. day] []
        pure
          ( openDayDate,
            [ (scheduledSlotStartTime, scheduledSlotEndTime)
              | Entity _ ScheduledSlot {..} <- slots
            ]
          )
    subscriptions <- runInPool pool getSubscriptions
    forM_ subscriptions $ \(TelegramUser suid lang _ _) -> do
      let langs = maybeToList lang
      sendMessage
        ( sendMessageRequest
            (ChatId (fromIntegral suid))
            ( tr langs (MsgScheduleIntro garageName garageAddress)
                <> "\n\n"
                <> renderSchedule langs openDaysWithSlots
            )
        )
          { sendMessageParseMode = Just MarkdownV2
          }

unlockSchedule :: [Lang] -> ConnectionPool -> ChatChannel -> ClientM ()
unlockSchedule langs pool ChatChannel {..} = do
  void $
    sendMessage
      (sendMessageRequest channelChatId (tr langs MsgUnlocked))
        { sendMessageParseMode = Just MarkdownV2
        }
  garages <- runInPool pool (selectList [] [])
  now <- localDay . zonedTimeToLocalTime <$> liftIO getZonedTime
  let nextWeek =
        Prelude.take 7 $ dropWhile ((/= Monday) . dayOfWeek) $ drop 1 [now ..]
  forM_ garages $ \(Entity gid _) -> do
    let selector = [OpenDayGarage ==. gid, OpenDayDate <-. nextWeek]
    runInPool pool $ updateWhere selector [OpenDayAvailable =. True]

knownLangs :: [Text]
knownLangs = ["en", "ru"]

volunteerCommands :: [(Text, BotMessage)]
volunteerCommands = [("list", MsgCommandList), ("signup", MsgCommandSignup), ("subscribe", MsgCommandSubscribe), ("unsubscribe", MsgCommandUnsubscribe), ("delete", MsgCommandDelete)]

adminCommands :: [(Text, BotMessage)]
adminCommands = [("setopendays", MsgCommandSetOpenDays), ("lock", MsgCommandLock)]

setCommands :: [(Text, BotMessage)] -> ChatChannel -> ClientM ()
setCommands commands ChatChannel {..} = forM_ knownLangs $ \lang -> do
  setMyCommands
    ( SetMyCommandsRequest
        { setMyCommandsCommands = fmap (\(c, d) -> BotCommand c (tr [lang] d)) commands,
          setMyCommandsScope = Just $ BotCommandScopeChat $ SomeChatId channelChatId,
          setMyCommandsLanguageCode = Just lang
        }
    )

bot :: ConnectionPool -> ChatChannel -> ClientM ()
bot pool chat@ChatChannel {channelChatId, channelUpdateChannel} = do
  upd <- getUpdate channelUpdateChannel
  let user =
        case upd of
          SomeNewMessage Message {messageFrom} -> messageFrom
          SomeNewCallbackQuery CallbackQuery {callbackQueryFrom} ->
            Just callbackQueryFrom
          _ -> Nothing
  Response {responseResult = Chat {chatType}} <-
    getChat (SomeChatId channelChatId)
  case chatType of
    ChatTypePrivate -> do
      let Just
            u@User
              { userId = UserId uid,
                userLanguageCode,
                userUsername,
                userFirstName,
                userLastName
              } = user
      Entity dbUserId TelegramUser {telegramUserLang} <-
        runInPool
          pool
          ( upsert
              ( TelegramUser
                  (fromIntegral uid)
                  userLanguageCode
                  (userFirstName <> maybe "" (" " <>) userLastName)
                  userUsername
              )
              [ TelegramUserLang =. userLanguageCode,
                TelegramUserFullName
                  =. (userFirstName <> maybe "" (" " <>) userLastName),
                TelegramUserUsername =. userUsername
              ]
          )
      let langs = maybeToList telegramUserLang
      runInPool pool (getBy (UniqueAdmin dbUserId)) >>= \case
        Just (Entity _ _) -> do
          setCommands adminCommands chat
          case upd of
            SomeNewMessage m@Message {..} ->
              case messageText of
                Just "/start" ->
                  void $
                    sendMessage
                      (sendMessageRequest channelChatId "Welcome, admin!")
                Just "/setopendays" -> makeSchedule langs pool chat
                Just "/lock" -> lockSchedule langs pool chat
                _ -> void $ reply m langs MsgNotRecognized
            SomeNewCallbackQuery
              CallbackQuery
                { callbackQueryData = Just txt,
                  callbackQueryMessage = Just Message {..},
                  callbackQueryId
                } -> do
                void $
                  answerCallbackQuery (answerCallbackQueryRequest callbackQueryId)
                case txt of
                  _
                    | "allow_" `isPrefixOf` txt -> do
                        allowVolunteer pool $
                          read $
                            unpack $
                              T.replace "allow_" "" txt
                        void $ deleteMessage channelChatId messageMessageId
                    | "decline_" `isPrefixOf` txt ->
                        void $ deleteMessage channelChatId messageMessageId
                    | "ban_" `isPrefixOf` txt ->
                        banVolunteer pool $ read $ unpack $ T.replace "ban_" "" txt
                    | "cancel_" `isPrefixOf` txt -> do
                        void $ deleteMessage channelChatId messageMessageId
                        cancelSlot pool $ readSqlKey $ T.replace "cancel_" "" txt
                    | txt == "setopendays" -> makeSchedule langs pool chat
                    | txt == "lock" -> lockSchedule langs pool chat
                    | txt == "unlock" -> unlockSchedule langs pool chat
                  _ -> pure ()
            _ -> pure ()
        _ -> do
          runInPool pool (getBy (UniqueVolunteer dbUserId)) >>= \case
            Just (Entity volunteer _) -> do
              setCommands volunteerCommands chat
              case upd of
                SomeNewMessage m@Message {..} ->
                  case messageText of
                    Just "/start" -> void $ reply m langs MsgVolunteer
                    Just "/signup" -> signUp langs chat volunteer pool
                    Just "/list" -> list langs chat volunteer pool
                    Just "/subscribe" -> subscribe langs chat dbUserId pool
                    Just "/unsubscribe" -> unsubscribe langs chat dbUserId pool
                    Just "/delete" ->
                      askDeleteUser langs chat volunteer pool
                    _ -> void $ reply m langs MsgNotRecognized
                SomeNewCallbackQuery
                  CallbackQuery
                    { callbackQueryData = Just txt,
                      callbackQueryMessage = Just Message {..},
                      callbackQueryId
                    } -> do
                    void $
                      answerCallbackQuery
                        (answerCallbackQueryRequest callbackQueryId)
                    case txt of
                      _
                        | "signup" `isPrefixOf` txt -> do
                            let (garageName, date) =
                                  read $ unpack $ T.replace "signup" "" txt
                            Just (Entity day _) <-
                              runInPool pool $ do
                                Just (Entity garage _) <-
                                  getBy $ UniqueGarage garageName
                                getBy $ UniqueDay date garage
                            Message {messageMessageId = msg} <-
                              responseResult
                                <$> sendMessage
                                  ( sendMessageRequest
                                      channelChatId
                                      (tr langs MsgSignup)
                                  )
                                    { sendMessageParseMode = Just MarkdownV2,
                                      sendMessageReplyMarkup =
                                        Just $ ik [[ikb "Cancel" "cancel"]]
                                    }
                            void $ existingSlotsStep langs pool chat volunteer day
                            void $ deleteMessage channelChatId msg
                        | "cancel_" `isPrefixOf` txt ->
                            void $
                              askCancelSlot langs pool chat messageMessageId $
                                readSqlKey $
                                  T.replace "cancel_" "" txt
                        | "confirm_" `isPrefixOf` txt ->
                            confirmSlot langs pool chat messageMessageId $
                              readSqlKey $
                                T.replace "confirm_" "" txt
                      _ -> pure ()
                _ -> pure ()
            Nothing ->
              case upd of
                SomeNewMessage m@Message {..} ->
                  case messageText of
                    Just "/start" ->
                      askForPermission pool u
                        >> void (send channelChatId langs MsgGreet)
                    _ -> void $ reply m langs MsgNotRecognized
                _ -> pure ()
    _ ->
      liftIO $ hPutStrLn stderr "Event from a non-private chat; Doing nothing"
