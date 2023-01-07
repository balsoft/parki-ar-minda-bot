{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

module Bot
  ( bot,
  )
where

import Control.Concurrent (threadDelay)
import Control.Monad (forM, forM_, forever, unless, void, when)
import Control.Monad.Except (MonadError (catchError))
import Control.Monad.IO.Unlift (MonadIO (liftIO))
import Control.Monad.Reader (ReaderT)
import Data.Functor (($>), (<&>))
import qualified Data.List as L
import Data.Maybe (catMaybes, fromJust, isJust, mapMaybe, maybeToList)
import Data.Text as T (Text, intercalate, isPrefixOf, pack, replace, splitOn, takeWhile, takeWhileEnd, unpack)
import Data.Text.IO (hPutStrLn, putStrLn)
import qualified Data.Text.IO as TIO
import Data.Time
import Data.Time.Calendar.OrdinalDate (mondayStartWeek)
import Database.Persist
import Database.Persist.Sqlite (ConnectionPool, SqlBackend)
import Debug.Trace (traceShowId)
import I18N
import Persist
import Servant.Client hiding (Response)
import Symbols
import System.IO (hPrint, stderr)
import Telegram.Bot.API
import Telegram.Bot.API (Message (messageMessageId))
import Telegram.Bot.Monadic
import Text.Shakespeare.I18N (Lang)
import Util

insertCallbackQueryMessage :: ConnectionPool -> Response Message -> ClientM (Response Message)
insertCallbackQueryMessage pool r@(Response {responseResult = Message {messageMessageId = MessageId mid, messageChat = Chat {chatId = ChatId cid}, messageReplyMarkup = Just (InlineKeyboardMarkup buttons)}}) = runInPool pool $ do
  forM_ (concat buttons) $ \(InlineKeyboardButton {..}) -> case inlineKeyboardButtonCallbackData of
    Nothing -> pure ()
    Just d -> void $ insert $ CallbackQueryMultiChat (fromIntegral cid) (fromIntegral mid) d
  pure r
insertCallbackQueryMessage _ r = pure r

deleteCallbackQueryMessages :: ConnectionPool -> Text -> ClientM ()
deleteCallbackQueryMessages pool d = do
  messages <- runInPool pool $ selectList [CallbackQueryMultiChatCallbackQuery ==. d] []
  forM_ messages $ \(Entity _ CallbackQueryMultiChat {..}) -> do
    void (deleteMessage (ChatId $ fromIntegral callbackQueryMultiChatChatId) (MessageId $ fromIntegral callbackQueryMultiChatMsgId))
      `catchError` const (pure ())
  runInPool pool $ deleteWhere [CallbackQueryMultiChatMsgId <-. fmap (callbackQueryMultiChatMsgId . entityVal) messages]

-- | Generate a grid of buttons for choosing a time
timeGrid ::
  OpenDayId ->
  Maybe Text ->
  -- | Don't show times earlier than this
  [[InlineKeyboardButton]]
timeGrid d s =
  fmap reverse $
    reverse $
      chunksOf 2 $
        reverse
          (fmap (\t -> ikb (showHourMinutes t) (label <> showSqlKey d <> "_" <> optionalStartTime <> showHourMinutes t)) times)
  where
    (timeLessThan, timeNoMoreThan) =
      case parseHourMinutesM =<< s of
        Just (TimeOfDay sHour sMinute _) ->
          ( (TimeOfDay (sHour + 1) sMinute 0 >),
            (TimeOfDay (sHour + 3) sMinute 0 >=)
          )
        Nothing -> (const False, const True)
    times =
      L.takeWhile timeNoMoreThan $
        dropWhile
          timeLessThan
          [TimeOfDay hour minute 0 | hour <- [9 .. 21], minute <- [0, 30]]
    label = if isJust s then "end_" else "start_"
    optionalStartTime = maybe "" (<> "_") s

cancelButton :: [Lang] -> InlineKeyboardButton
cancelButton langs = ikb (tr langs MsgCancel) "cancel"

menuStep ::
  ChatChannel -> Maybe MessageId -> Text -> [[InlineKeyboardButton]] -> ClientM MessageId
menuStep ChatChannel {..} Nothing msg grid =
  messageMessageId . responseResult
    <$> sendMessage
      (sendMessageRequest channelChatId msg)
        { sendMessageReplyMarkup = Just $ ik grid,
          sendMessageParseMode = Just MarkdownV2
        }
menuStep ChatChannel {..} (Just msgId) msg grid =
  catchError
    ( do
        void (editMessageText (editMessageTextRequest msg) {editMessageTextMessageId = Just msgId, editMessageTextChatId = Just $ SomeChatId channelChatId, editMessageTextParseMode = Just MarkdownV2})
        void (editMessageReplyMarkup (editMessageReplyMarkupRequest (Just $ ik grid)) {editMessageReplyMarkupMessageId = Just msgId, editMessageReplyMarkupChatId = Just $ SomeChatId channelChatId})
    )
    (const $ pure ())
    $> msgId

garageStep ::
  [Lang] ->
  ConnectionPool ->
  ChatChannel ->
  Maybe MessageId ->
  ClientM MessageId
garageStep langs pool chat@ChatChannel {..} msgId = do
  garages <- runInPool pool $ selectList [] []
  let grid =
        [ [ikb garageName ("garage_" <> showSqlKey gid)]
          | Entity gid (Garage {..}) <- garages
        ]
  menuStep chat msgId (tr langs MsgChooseGarage) (grid <> [[cancelButton langs]])

dayStep ::
  [Lang] ->
  ConnectionPool ->
  ChatChannel ->
  Maybe MessageId ->
  VolunteerId ->
  GarageId ->
  ClientM MessageId
dayStep langs pool chat@ChatChannel {..} msgId volunteer garage = do
  now <- localDay . zonedTimeToLocalTime <$> liftIO getZonedTime
  days <-
    runInPool pool $
      mapM
        ( \e@(Entity d OpenDay {openDayDate}) -> do
            otherPeople <-
              selectList
                [ScheduledSlotDay ==. d, ScheduledSlotUser !=. volunteer]
                []
            me <-
              concat
                <$> ( mapM
                        ( \(Entity d' _) ->
                            selectList
                              [ScheduledSlotDay ==. d', ScheduledSlotUser ==. volunteer]
                              []
                        )
                        =<< selectList [OpenDayDate ==. openDayDate] []
                    )
            pure (e, not $ null otherPeople, not $ null me)
        )
        =<< selectList
          [OpenDayGarage ==. garage, OpenDayDate >. now, OpenDayAvailable ==. True]
          []
  let anotherGarage = [[ikb (house <> tr langs MsgChangeGarage) "signup"]]
  if null days
    then do
      menuStep chat msgId (forbidden <> tr langs MsgNoDays) (anotherGarage <> [[cancelButton langs]])
    else do
      let grid =
            [ [ ikb
                  ( showDay langs openDayDate
                      <> ( if otherPeople
                             then " " <> people
                             else ""
                         )
                      <> ( if me
                             then " " <> diamond
                             else ""
                         )
                  )
                  ("day_" <> showSqlKey did)
              ]
              | (Entity did (OpenDay {..}), otherPeople, me) <- days
            ]
      menuStep chat msgId (tr langs MsgChooseDay) (grid <> anotherGarage <> [[cancelButton langs]])

getMySlots :: (MonadIO m, MonadFail m) => [Lang] -> Key OpenDay -> VolunteerId -> ReaderT SqlBackend m [Text]
getMySlots langs day volunteer = do
  Just OpenDay {..} <- get day
  mapM (getSlotDesc langs . entityVal) . concat
    =<< ( mapM
            ( \(Entity d' _) ->
                selectList
                  [ScheduledSlotDay ==. d', ScheduledSlotUser ==. volunteer]
                  []
            )
            =<< selectList [OpenDayDate ==. openDayDate] []
        )

mySlotsMsg ::
  MonadIO m =>
  [Lang] ->
  ConnectionPool ->
  Key OpenDay ->
  VolunteerId ->
  m Text
mySlotsMsg langs pool day volunteer = runInPool pool $ do
  mySlots <- getMySlots langs day volunteer
  pure $
    if not $ null mySlots
      then
        "\n\n"
          <> diamond
          <> " "
          <> tr langs MsgOtherDutiesThisDay
          <> "\n\n"
          <> intercalate "\n\n" mySlots
      else ""

othersSlots :: (MonadIO m, MonadFail m) =>
  OpenDayId ->
  VolunteerId ->
  ReaderT SqlBackend m [(Entity ScheduledSlot, Text)]
othersSlots day volunteer =
  mapM
    ( \e@(Entity _ ScheduledSlot {scheduledSlotUser}) -> do
        Just Volunteer {volunteerUser} <- get scheduledSlotUser
        Just TelegramUser {..} <- get volunteerUser
        pure (e, maybe telegramUserFullName ("@" <>) telegramUserUsername)
    )
    =<< selectList
      [ScheduledSlotDay ==. day, ScheduledSlotUser !=. volunteer]
      []

existingSlotsStep ::
  [Lang] ->
  ConnectionPool ->
  ChatChannel ->
  Maybe MessageId ->
  VolunteerId ->
  OpenDayId ->
  ClientM MessageId
existingSlotsStep langs pool chat@ChatChannel {..} msgId volunteer day = do
  Just OpenDay {openDayAvailable, openDayGarage} <- runInPool pool $ get day
  let anotherDay = [[ikb (tr langs MsgChangeDay) ("garage_" <> showSqlKey openDayGarage)]]
  if not openDayAvailable
    then do
      menuStep chat msgId (forbidden <> tr langs MsgDayUnavailable) (anotherDay <> [[cancelButton langs]])
    else do
      slots <- runInPool pool $ othersSlots day volunteer
      if null slots
        then startTimeStep langs pool chat msgId volunteer day
        else do
          mySlots <- mySlotsMsg langs pool day volunteer
          let grid =
                [ [ ikb
                      ( showHourMinutes scheduledSlotStartTime
                          <> "-"
                          <> showHourMinutes scheduledSlotEndTime
                          <> ", "
                          <> name
                      )
                      ("end_" <> showSqlKey scheduledSlotDay <> "_" <> showHourMinutes scheduledSlotStartTime <> "_" <> showHourMinutes scheduledSlotEndTime)
                  ]
                  | (Entity sid ScheduledSlot {..}, name) <- slots
                ]
                  ++ [ [ ikb
                           ( tr langs MsgNewAppointment
                           )
                           ("new_" <> showSqlKey day)
                       ]
                     ]
          menuStep
            chat
            msgId
            (people <> " " <> tr langs MsgOtherVolunteers <> " " <> tr langs MsgChooseExistingSlot <> mySlots)
            (grid <> [[ikb (calendar <> tr langs MsgChangeDay) ("garage_" <> showSqlKey openDayGarage)], [cancelButton langs]])

startTimeStep ::
  [Lang] ->
  ConnectionPool ->
  ChatChannel ->
  Maybe MessageId ->
  VolunteerId ->
  OpenDayId ->
  ClientM MessageId
startTimeStep langs pool chat@ChatChannel {..} msgId volunteer day = do
  Just OpenDay {openDayGarage} <- runInPool pool $ get day
  mySlots <- mySlotsMsg langs pool day volunteer
  menuStep
    chat
    msgId
    (clock <> tr langs MsgChooseStartTime <> mySlots)
    (timeGrid day Nothing <> [[ikb (calendar <> tr langs MsgChangeDay) ("garage_" <> showSqlKey openDayGarage)], [cancelButton langs]])

endTimeStep ::
  [Lang] ->
  ConnectionPool ->
  ChatChannel ->
  Maybe MessageId ->
  OpenDayId ->
  Text ->
  ClientM MessageId
endTimeStep langs pool chat@ChatChannel {..} msgId day startTime = do
  menuStep
    chat
    msgId
    (clock <> tr langs MsgChooseEndTime)
    (timeGrid day (Just startTime) <> [[ikb (clock <> tr langs MsgChangeStartTime) ("day_" <> showSqlKey day)], [cancelButton langs]])

timesIntersect :: Ord a => (a, a) -> (a, a) -> Bool
timesIntersect (sa, ea) (sb, eb) =
  sa <= sb && sb <= ea
    || sa <= eb && eb <= sa
    || sb <= sa && sa <= eb
    || sb <= ea && ea <= eb

slotsIntersect :: ScheduledSlot -> ScheduledSlot -> Bool
slotsIntersect
  ScheduledSlot
    { scheduledSlotDay = da,
      scheduledSlotStartTime = sa,
      scheduledSlotEndTime = ea
    }
  ScheduledSlot
    { scheduledSlotDay = db,
      scheduledSlotStartTime = sb,
      scheduledSlotEndTime = eb
    } =
    da == db && timesIntersect (sa, ea) (sb, eb)

askCreateStep ::
  [Lang] ->
  ConnectionPool ->
  ChatChannel ->
  Maybe MessageId ->
  VolunteerId ->
  OpenDayId ->
  TimeOfDay ->
  TimeOfDay ->
  ClientM MessageId
askCreateStep langs pool chat@ChatChannel {..} msgId volunteer day startTime endTime = do
  otherDuties <- runInPool pool $ getMySlots langs day volunteer
  others <- runInPool pool $ othersSlots day volunteer
  let othersMessage = if null others then "" else "\n\n" <> people <> " " <> tr langs MsgOtherVolunteers <> "\n\n" <> intercalate "\n" [ v <> "\\: " <> showHourMinutes scheduledSlotStartTime <> "—" <> showHourMinutes scheduledSlotEndTime | (Entity _ ScheduledSlot {..}, v) <- others]
  Just OpenDay {openDayGarage} <- runInPool pool $ get day
  mySlots <- mySlotsMsg langs pool day volunteer
  let extraButtons = [[ikb (clock <> tr langs MsgChangeStartTime) ("day_" <> showSqlKey day)], [ikb (calendar <> tr langs MsgChangeDay) ("garage_" <> showSqlKey openDayGarage)], [ikb (house <> tr langs MsgChangeGarage) "signup"], [cancelButton langs]]
  slotDesc <- runInPool pool $ getSlotDesc langs (ScheduledSlot {scheduledSlotDay = day, scheduledSlotStartTime = startTime, scheduledSlotEndTime = endTime, scheduledSlotState = ScheduledSlotCreated, scheduledSlotReminderSent = False, scheduledSlotUser = volunteer})
  if not $ null otherDuties
    then do
      menuStep
        chat
        msgId
        ( forbidden
            <> tr langs MsgOtherDuties
            <> "\n\n"
            <> slotDesc
            <> "\n\n"
            <> diamond
            <> tr langs MsgConflicts
            <> "\n\n"
            <> intercalate "\n\n" otherDuties
            <> othersMessage
        )
        ([[ikb (attention <> tr langs MsgCancelCreate) ("create_cancel_" <> showSqlKey day <> "_" <> showHourMinutes startTime <> "_" <> showHourMinutes endTime)]] <> extraButtons)
    else do
      menuStep chat msgId (tr langs MsgCreate <> "\n" <> slotDesc <> mySlots <> othersMessage) ([[ikb (allGood <> tr langs MsgYesCreate) ("create_" <> showSqlKey day <> "_" <> showHourMinutes startTime <> "_" <> showHourMinutes endTime)]] <> extraButtons)

cancelSlotButton :: [Lang] -> ScheduledSlotId -> InlineKeyboardButton
cancelSlotButton langs slotId =
  ikb (tr langs MsgCantCome) ("cancel_" <> showSqlKey slotId)

slotCreated ::
  [Lang] ->
  ConnectionPool ->
  ChatChannel ->
  Maybe MessageId ->
  VolunteerId ->
  OpenDayId ->
  TimeOfDay ->
  TimeOfDay ->
  ClientM MessageId
slotCreated langs pool chat@ChatChannel {..} msgId volunteer day startTime endTime = do
  slotId <- runInPool pool (insert (ScheduledSlot {scheduledSlotDay = day, scheduledSlotStartTime = startTime, scheduledSlotEndTime = endTime, scheduledSlotUser = volunteer, scheduledSlotState = ScheduledSlotCreated, scheduledSlotReminderSent = False}))
  Just slot <- runInPool pool $ get slotId
  slotDesc <- runInPool pool $ getSlotDesc langs slot
  Just OpenDay {openDayGarage, openDayDate} <- runInPool pool $ get (scheduledSlotDay slot)
  void $
    menuStep
      chat
      msgId
      (allGood <> tr langs MsgCreated <> "\n" <> slotDesc)
      [[cancelSlotButton langs slotId]]
  menuStep chat Nothing (tr langs MsgAnotherSlot) [[ikb (tr langs MsgAnotherGarage) "signup"], [ikb (tr langs MsgSameGarage) ("garage_" <> showSqlKey openDayGarage)], [ikb (tr langs MsgDone) "cancel"]]
    <* updateWorkingSchedule pool False (thisWeekStart openDayDate) openDayGarage
    `catchError` (liftIO . print)

signUp :: [Lang] -> ChatChannel -> VolunteerId -> ConnectionPool -> ClientM ()
signUp langs chat@ChatChannel {channelChatId = channelChatId@(ChatId _cid)} volunteer pool = do
  void $ garageStep langs pool chat Nothing

list :: [Lang] -> ChatChannel -> VolunteerId -> ConnectionPool -> ClientM ()
list langs ChatChannel {channelChatId} volunteer pool = do
  today <- localDay . zonedTimeToLocalTime <$> liftIO getZonedTime
  slots <-
    runInPool pool $ do
      days <- fmap entityKey <$> selectList [OpenDayDate >=. today] []
      selectList [ScheduledSlotUser ==. volunteer, ScheduledSlotDay <-. days] []
  when (null slots) $ void $ send channelChatId langs MsgNoSlots
  forM_ slots $ \(Entity slotId slot) -> do
    slotDesc <- runInPool pool $ getSlotDesc langs slot
    sendMessage
      (sendMessageRequest channelChatId slotDesc)
        { sendMessageParseMode = Just MarkdownV2,
          sendMessageReplyMarkup = Just $ ik [[cancelSlotButton langs slotId]]
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
  [Lang] -> ChatChannel -> VolunteerId -> ConnectionPool -> ClientM ()
askDeleteUser langs chat@ChatChannel {channelChatId} volunteer pool = do
  void $
    sendMessage
      (sendMessageRequest channelChatId (attention <> tr langs MsgAreYouSure))
  untilRight (getNewMessage chat) (const $ pure ()) >>= \case
    Message {messageText = Just txt}
      | txt == tr langs MsgIAmSure -> do
          deleteUser pool volunteer
          void $ send channelChatId langs MsgDeleted
    _ -> void $ send channelChatId langs MsgNotDeleting

cancelSlot :: ConnectionPool -> ScheduledSlotId -> ClientM ()
cancelSlot pool slotId = do
  (slot, slotDesc, dayAvailable, langs, TelegramUser {..}, gid, weekStart) <-
    runInPool pool $ do
      Just slot <- get slotId
      Just Volunteer {..} <- get $ scheduledSlotUser slot
      Just OpenDay {..} <- get $ scheduledSlotDay slot
      Just user@TelegramUser {..} <- get volunteerUser
      let langs = maybeToList telegramUserLang
      let weekStart = thisWeekStart openDayDate
      desc <- getSlotDesc langs slot
      pure (slot, desc, openDayAvailable, langs, user, openDayGarage, weekStart)
  runInPool pool $ delete slotId
  void $
    sendMessage
      ( sendMessageRequest
          (ChatId (fromIntegral telegramUserUserId))
          (attention <> tr langs MsgYourSlotCancelled <> "\n" <> slotDesc)
      )
        { sendMessageParseMode = Just MarkdownV2
        }
  updateWorkingSchedule pool False weekStart gid `catchError` (liftIO . print)
  admins <- runInPool pool getAdmins
  unless dayAvailable $
    forM_ admins $ \(TelegramUser auid lang _ _) -> do
      let adminLangs = maybeToList lang
      slotFullDesc <- runInPool pool $ getSlotFullDesc adminLangs slot
      void $
        sendMessage
          ( sendMessageRequest
              (ChatId (fromIntegral auid))
              (attention <> tr adminLangs MsgSlotCancelled <> "\n" <> slotFullDesc)
          )
            { sendMessageParseMode = Just MarkdownV2,
              sendMessageReplyMarkup =
                Just (ik [[ikb (tr adminLangs MsgLock) ("lock_" <> pack (showGregorian weekStart))]])
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
  runInPool pool $ update slotId [ScheduledSlotState =. ScheduledSlotConfirmed]
  void $
    sendMessage
      ( sendMessageRequest
          channelChatId
          (allGood <> tr langs MsgConfirmedShort <> "\n" <> slotDesc)
      )
        { sendMessageParseMode = Just MarkdownV2,
          sendMessageReplyMarkup = Just $ ik [[cancelSlotButton langs slotId]]
        }

askForPermission :: ConnectionPool -> User -> ClientM ()
askForPermission pool User {userId = UserId uid} = do
  admins <- runInPool pool getAdmins
  Just (Entity tuid user) <-
    runInPool pool (getBy $ UniqueTelegramUser (fromIntegral uid))
  forM_ admins $ \(TelegramUser auid lang _ _) -> do
    let langs = maybeToList lang
    void $
      insertCallbackQueryMessage pool
        =<< sendMessage
          ( sendMessageRequest
              (ChatId (fromIntegral auid))
              (tr langs $ MsgVolunteerRequest $ renderUser user)
          )
            { sendMessageParseMode = Just MarkdownV2,
              sendMessageReplyMarkup =
                Just $
                  ik
                    [ [ ikb
                          (allGood <> tr langs MsgAllow)
                          ("allow_" <> showSqlKey tuid),
                        ikb
                          (bad <> tr langs MsgDecline)
                          ("decline_" <> showSqlKey tuid)
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
      insertCallbackQueryMessage pool
        =<< sendMessage
          ( sendMessageRequest
              (ChatId (fromIntegral auid))
              (tr langs $ MsgNewVolunteer $ renderUser user)
          )
            { sendMessageParseMode = Just MarkdownV2,
              sendMessageReplyMarkup =
                Just $
                  ik
                    [ [ ikb
                          (forbidden <> tr langs MsgBan)
                          ("ban_" <> showSqlKey tuid)
                      ]
                    ]
            }
  deleteCallbackQueryMessages pool ("allow_" <> showSqlKey tuid)
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
  deleteCallbackQueryMessages pool ("ban_" <> showSqlKey tuid)
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
  getPollResult initial <* deleteMessage channelChatId (messageMessageId msg)

getSubscriptions :: MonadIO m => ReaderT SqlBackend m [TelegramUser]
getSubscriptions = do
  admins <- selectList [] []
  subscriptions <- selectList [] []
  let ids =
        fmap (adminUser . entityVal) admins
          ++ fmap (subscriptionUser . entityVal) subscriptions
  catMaybes <$> mapM get ids

showSchedule :: [Lang] -> Text -> [(Day, [(TelegramUser, TimeOfDay, TimeOfDay)])] -> Text
showSchedule langs garageName days =
  calendar
    <> tr langs (MsgWorkingScheduleFor garageName)
    <> "\n\n"
    <> intercalate
      "\n\n"
      [ "*"
          <> showDay langs day
          <> "*\n"
          <> intercalate
            "\n"
            [ clock <> renderUser user <> ": " <> showHourMinutes start <> "-" <> showHourMinutes end | (user, start, end) <- slots
            ]
        | (day, slots) <- days
      ]

updateWorkingSchedule :: ConnectionPool -> Bool -> Day -> GarageId -> ClientM ()
updateWorkingSchedule pool recreate weekStart garage = do
  admins <- runInPool pool (selectList [] [] >>= mapM (get . adminUser . entityVal))
  let s = "working_schedule_" <> showSqlKey garage <> "_" <> pack (showGregorian weekStart)
  Just Garage {..} <- runInPool pool $ get garage
  let selector = [OpenDayGarage ==. garage, OpenDayDate <-. take 7 [weekStart ..]]
  openDaysWithSlots <-
    runInPool pool $
      selectList selector []
        >>= mapM
          ( \(Entity day OpenDay {..}) -> do
              slots <- runInPool pool $ selectList [ScheduledSlotDay ==. day] []
              times <- forM slots $ \(Entity _ ScheduledSlot {..}) -> do
                Just Volunteer {..} <- get scheduledSlotUser
                Just u <- get volunteerUser
                pure (u, scheduledSlotStartTime, scheduledSlotEndTime)
              pure (openDayDate, times)
          )
  forM_ admins $ \(Just (TelegramUser auid lang _ _)) -> do
    let langs = maybeToList lang
    let t = escapeMd $ showSchedule langs garageName openDaysWithSlots
    messages <- runInPool pool $ selectList [CallbackQueryMultiChatCallbackQuery ==. s, CallbackQueryMultiChatChatId ==. auid] []
    flip catchError (const $ pure ()) $ case (recreate, messages) of
      (False, [Entity _ CallbackQueryMultiChat {..}]) -> do
        flip catchError (liftIO . print) $
          void $
            editMessageText
              (editMessageTextRequest t)
                { editMessageTextMessageId = Just (MessageId $ fromIntegral callbackQueryMultiChatMsgId),
                  editMessageTextChatId = Just $ SomeChatId (ChatId $ fromIntegral auid),
                  editMessageTextParseMode = Just MarkdownV2
                }
      (_, msgs) -> do
        MessageId mid <- messageMessageId . responseResult <$> sendMessage (sendMessageRequest (ChatId (fromIntegral auid)) t) {sendMessageParseMode = Just MarkdownV2}
        void $ runInPool pool $ insert $ CallbackQueryMultiChat auid (fromIntegral mid) s
        forM_ msgs $ \(Entity _ CallbackQueryMultiChat {..}) -> do
          flip catchError (liftIO . print) $ void $ deleteMessage (ChatId (fromIntegral auid)) (MessageId $ fromIntegral callbackQueryMultiChatMsgId)
          runInPool pool $ deleteWhere [CallbackQueryMultiChatChatId ==. auid, CallbackQueryMultiChatMsgId ==. callbackQueryMultiChatMsgId]

sendOpenDaySchedule :: ConnectionPool -> Day -> GarageId -> [Entity OpenDay] -> ClientM ()
sendOpenDaySchedule pool weekStart garage days = do
  subs <- runInPool pool (selectList [] [] >>= mapM (get . subscriptionUser . entityVal))
  Just Garage {..} <- runInPool pool $ get garage
  updateWorkingSchedule pool True weekStart garage `catchError` (liftIO . print)
  forM_ subs $ \(Just (TelegramUser uid lang _ _)) -> do
    let langs = maybeToList lang
    catchError
      ( do
          Response {responseResult = Message {messageMessageId = MessageId mid}} <-
            sendMessage
              ( sendMessageRequest
                  (ChatId (fromIntegral uid))
                  ( news
                      <> tr langs (MsgNewScheduleFor garageName)
                  )
              )
                { sendMessageParseMode = Just MarkdownV2,
                  sendMessageReplyMarkup =
                    Just $
                      ik
                        [ [ ikb
                              (showDay langs openDayDate)
                              ("signup_" <> showSqlKey did)
                          ]
                          | Entity did OpenDay {..} <- days
                        ]
                }
          void $ runInPool pool $ insert $ CallbackQueryMultiChat uid (fromIntegral mid) ("schedule_" <> pack (showGregorian weekStart))
      )
      (liftIO . hPrint stderr)

makeSchedule :: [Lang] -> ConnectionPool -> ChatChannel -> Maybe Day -> ClientM ()
makeSchedule langs pool chat day' = do
  day <- liftIO $ case day' of
    Just d -> pure d
    Nothing -> nextWeekStart . localDay . zonedTimeToLocalTime <$> getZonedTime
  garages <- runInPool pool (selectList [] [])
  let nextWeek = take 7 [day ..]
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
                days <- runInPool pool $ do
                  deleteWhere [DefaultOpenDayGarage ==. gid]
                  mapM_ (insert . DefaultOpenDay gid) (fmap dayOfWeek days)
                  deleteWhere [OpenDayGarage ==. gid, OpenDayDate /<-. days]
                  mapM
                    ( \day ->
                        upsertBy
                          (UniqueDay day gid)
                          (OpenDay gid day True)
                          [OpenDayAvailable =. True]
                    )
                    days
                sendOpenDaySchedule pool (head nextWeek) gid days
          )
        . fmap (L.sort . mapMaybe parseGregorian)

-- | Merge intersecting intervals together
-- | This assumes that all intervals are valid, i.e. (start, end) | start <= end
mergeIntervals :: Ord a => [(a, a)] -> [(a, a)]
mergeIntervals slots = go (L.sort slots)
  where
    go [] = []
    go [a] = [a]
    go ((sa, ea) : (sb, eb) : xs) = if sb <= ea then (sa, eb) : go xs else (sa, ea) : (sb, eb) : go xs

renderSchedule :: [Lang] -> [(Day, [(TimeOfDay, TimeOfDay)])] -> Text
renderSchedule langs s =
  intercalate
    "\n"
    [ escapeMd (showDay langs day)
        <> "\n\n"
        <> intercalate
          "\n"
          [tr langs (MsgScheduleTime (showHourMinutes start) (showHourMinutes end)) | (start, end) <- mergeIntervals times]
      | (day, times) <- L.sort s,
        times /= []
    ]

lockSchedule :: [Lang] -> ConnectionPool -> ChatChannel -> Maybe Day -> ClientM ()
lockSchedule langs pool ChatChannel {..} day' = do
  day <- liftIO $ case day' of
    Just d -> pure d
    Nothing -> nextWeekStart . localDay . zonedTimeToLocalTime <$> getZonedTime
  deleteCallbackQueryMessages pool ("schedule_" <> pack (showGregorian day))
  void $
    sendMessage
      (sendMessageRequest channelChatId (tr langs MsgLocked))
        { sendMessageParseMode = Just MarkdownV2,
          sendMessageReplyMarkup =
            Just $ ik [[ikb (tr langs MsgUnlock) ("unlock_" <> pack (showGregorian day))]]
        }
  garages <- runInPool pool (selectList [] [])
  let nextWeek = take 7 [day ..]
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
      let subscriptionLangs = maybeToList lang
      sendMessage
        ( sendMessageRequest
            (ChatId (fromIntegral suid))
            ( tr subscriptionLangs (MsgScheduleIntro garageName garageAddress)
                <> "\n\n"
                <> renderSchedule subscriptionLangs openDaysWithSlots
            )
        )
          { sendMessageParseMode = Just MarkdownV2
          }

unlockSchedule :: [Lang] -> ConnectionPool -> ChatChannel -> Maybe Day -> ClientM ()
unlockSchedule langs pool ChatChannel {..} day' = do
  day <- liftIO $ case day' of
    Just d -> pure d
    Nothing -> nextWeekStart . localDay . zonedTimeToLocalTime <$> getZonedTime
  void $
    sendMessage
      (sendMessageRequest channelChatId (tr langs MsgUnlocked))
        { sendMessageParseMode = Just MarkdownV2
        }
  garages <- runInPool pool (selectList [] [])
  let nextWeek = take 7 [day ..]
  forM_ garages $ \(Entity gid Garage {..}) -> do
    let selector = [OpenDayGarage ==. gid, OpenDayDate <-. nextWeek]
    days <- runInPool pool (updateWhere selector [OpenDayAvailable =. True] >> selectList selector [])
    sendOpenDaySchedule pool (head nextWeek) gid days

knownLangs :: [Text]
knownLangs = ["en", "ru"]

volunteerCommands :: [(Text, BotMessage)]
volunteerCommands =
  [ ("list", MsgCommandList),
    ("signup", MsgCommandSignup),
    ("subscribe", MsgCommandSubscribe),
    ("unsubscribe", MsgCommandUnsubscribe),
    ("delete", MsgCommandDelete)
  ]

adminCommands :: [(Text, BotMessage)]
adminCommands =
  [("setopendays", MsgCommandSetOpenDays), ("lock", MsgCommandLock)]

setCommands :: [(Text, BotMessage)] -> ChatChannel -> ClientM ()
setCommands commands ChatChannel {..} =
  forM_ knownLangs $ \lang -> do
    setMyCommands
      ( SetMyCommandsRequest
          { setMyCommandsCommands =
              fmap (\(c, d) -> BotCommand c (tr [lang] d)) commands,
            setMyCommandsScope =
              Just $ BotCommandScopeChat $ SomeChatId channelChatId,
            setMyCommandsLanguageCode = Just lang
          }
      )

bot :: ConnectionPool -> ChatChannel -> ClientM ()
bot pool chat@ChatChannel {channelChatId, channelUpdateChannel} = forever $ do
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
      Just (Entity dbUserId TelegramUser {telegramUserLang}) <-
        runInPool
          pool
          ( putMany
              [ TelegramUser
                  (fromIntegral uid)
                  userLanguageCode
                  (userFirstName <> maybe "" (" " <>) userLastName)
                  userUsername
              ]
              >> getBy (UniqueTelegramUser (fromIntegral uid))
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
                Just "/setopendays" -> makeSchedule langs pool chat Nothing
                Just "/lock" -> lockSchedule langs pool chat Nothing
                Just txt | "/cancel_" `isPrefixOf` txt -> cancelSlot pool $ readSqlKey $ T.replace "/cancel_" "" txt
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
                          readSqlKey $
                            T.replace "allow_" "" txt
                        void $ deleteMessage channelChatId messageMessageId
                    | "decline_" `isPrefixOf` txt ->
                        void $ deleteMessage channelChatId messageMessageId
                    | "ban_" `isPrefixOf` txt ->
                        banVolunteer pool $ read $ unpack $ T.replace "ban_" "" txt
                    | "cancel_" `isPrefixOf` txt -> do
                        void $ deleteMessage channelChatId messageMessageId
                        cancelSlot pool $ readSqlKey $ T.replace "cancel_" "" txt
                    | "setopendays_" `isPrefixOf` txt -> makeSchedule langs pool chat $ parseGregorian (T.replace "setopendays_" "" txt)
                    | "lock_" `isPrefixOf` txt -> lockSchedule langs pool chat $ parseGregorian (T.replace "lock_" "" txt)
                    | "unlock_" `isPrefixOf` txt -> unlockSchedule langs pool chat $ parseGregorian (T.replace "unlock_" "" txt)
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
                    Just "/delete" -> askDeleteUser langs chat volunteer pool
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
                        | "signup_" `isPrefixOf` txt -> do
                            let day = readSqlKey $ T.replace "signup_" "" txt
                            void $ existingSlotsStep langs pool chat Nothing volunteer day
                        | txt == "cancel" -> void $ deleteMessage channelChatId messageMessageId
                        | txt == "signup" -> void $ garageStep langs pool chat (Just messageMessageId)
                        | "garage_" `isPrefixOf` txt -> void $ dayStep langs pool chat (Just messageMessageId) volunteer $ readSqlKey $ T.replace "garage_" "" txt
                        | "day_" `isPrefixOf` txt -> void $ existingSlotsStep langs pool chat (Just messageMessageId) volunteer $ readSqlKey $ T.replace "day_" "" txt
                        | "new_" `isPrefixOf` txt -> void $ startTimeStep langs pool chat (Just messageMessageId) volunteer $ readSqlKey $ T.replace "new_" "" txt
                        | "start_" `isPrefixOf` txt -> let [_, d, s] = T.splitOn "_" txt in void $ endTimeStep langs pool chat (Just messageMessageId) (readSqlKey d) s
                        | "end_" `isPrefixOf` txt -> let [_, d, s, e] = T.splitOn "_" txt in void $ askCreateStep langs pool chat (Just messageMessageId) volunteer (readSqlKey d) (fromJust $ parseHourMinutesM s) (fromJust $ parseHourMinutesM e)
                        | "create_cancel_" `isPrefixOf` txt ->
                            let [_, _, d, s, e] = T.splitOn "_" txt
                             in do
                                  startTime <- parseHourMinutesM s
                                  endTime <- parseHourMinutesM e
                                  slots <-
                                    runInPool pool $ do
                                      Just OpenDay {openDayDate} <- get $ readSqlKey d
                                      days <- selectList [OpenDayDate ==. openDayDate] []
                                      conflicts <- forM days $ \(Entity day _) ->
                                        selectList [ScheduledSlotDay ==. day, ScheduledSlotUser ==. volunteer] []
                                          <&> filter (\(Entity _ ScheduledSlot {..}) -> timesIntersect (scheduledSlotStartTime, scheduledSlotEndTime) (startTime, endTime))
                                      pure $ concat conflicts
                                  forM_ slots (cancelSlot pool . entityKey)
                                  void $ slotCreated langs pool chat (Just messageMessageId) volunteer (readSqlKey d) startTime endTime
                        | "create_" `isPrefixOf` txt -> let [_, d, s, e] = T.splitOn "_" txt in void $ slotCreated langs pool chat (Just messageMessageId) volunteer (readSqlKey d) (fromJust $ parseHourMinutesM s) (fromJust $ parseHourMinutesM e)
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
  liftIO $ threadDelay 1500000