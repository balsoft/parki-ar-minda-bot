{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-record-wildcards #-}

module Bot
  ( bot,
    updateWorkingScheduleForDay,
  )
where

import Control.Concurrent (threadDelay)
import Control.Monad (forM, forM_, forever, unless, void, when)
import Control.Monad.Except (MonadError (catchError))
import Control.Monad.IO.Unlift (MonadIO (liftIO))
import Control.Monad.Reader (ReaderT)
import Data.Functor (($>), (<&>))
import Data.List ((\\))
import qualified Data.List as L
import Data.Maybe (catMaybes, fromJust, isJust, mapMaybe, maybeToList)
import Data.Text as T (Text, intercalate, pack, splitOn, stripPrefix, unpack)
import Data.Text.IO (hPutStrLn)
import Data.Text.Lazy (toStrict)
import Data.Time
import Database.Persist
import Database.Persist.Sqlite (ConnectionPool, SqlBackend)
import I18N
import Persist
import Safe (readMay)
import Servant.Client hiding (Response)
import Symbols
import System.IO (hPrint, stderr)
import Telegram.Bot.API
import Telegram.Bot.Monadic
import Text.Blaze.Html (Html)
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Text.Hamlet (ihamlet)
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
          sendMessageParseMode = Just HTML
        }
menuStep ChatChannel {..} (Just msgId) msg grid =
  catchError
    ( do
        void
          ( editMessageText
              (editMessageTextRequest msg)
                { editMessageTextMessageId = Just msgId,
                  editMessageTextChatId = Just $ SomeChatId channelChatId,
                  editMessageTextParseMode = Just HTML
                }
          )
        void
          ( editMessageReplyMarkup
              (editMessageReplyMarkupRequest (Just $ ik grid))
                { editMessageReplyMarkupMessageId = Just msgId,
                  editMessageReplyMarkupChatId = Just $ SomeChatId channelChatId
                }
          )
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
  let anotherGarage = [[ikb (house <> " " <> tr langs MsgChangeGarage) "signup"]]
  if null days
    then do
      menuStep chat msgId (defaultRender langs [ihamlet|#{forbidden} _{MsgNoDays}|]) (anotherGarage <> [[cancelButton langs]])
    else do
      Just g <- runInPool pool $ get garage
      let weekStart = thisWeekStart (openDayDate $ entityVal $ (\(a, _, _) -> a) $ head days)
      t <- renderWorkingSchedule langs g <$> getWorkingSchedule pool weekStart garage
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
      menuStep
        chat
        msgId
        (tr langs MsgChooseDay <> "\n\n" <> t)
        (grid <> anotherGarage <> [[cancelButton langs]])

getMySlots :: (MonadIO m, MonadFail m) => [Lang] -> Key OpenDay -> VolunteerId -> ReaderT SqlBackend m [Html]
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
  m Html
mySlotsMsg langs pool day volunteer = runInPool pool $ do
  mySlots <- getMySlots langs day volunteer
  pure $
    defaultLayout
      langs
      [ihamlet|
    $if not (null mySlots)
      \
      \
      #{diamond} _{MsgOtherDutiesThisDay}
      $forall slot <- mySlots
        \
        \
        #{slot}
    $else
  |]

othersSlots ::
  (MonadIO m, MonadFail m) =>
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
      menuStep chat msgId (defaultRender langs [ihamlet|#{forbidden} _{MsgDayUnavailable}|]) (anotherDay <> [[cancelButton langs]])
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
                  | (Entity _ ScheduledSlot {..}, name) <- slots
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
            (toStrict $ renderHtml $ defaultLayout langs [ihamlet|#{people} _{MsgOtherVolunteers} _{MsgChooseExistingSlot}#{mySlots}|])
            (grid <> [[ikb (calendar <> " " <> tr langs MsgChangeDay) ("garage_" <> showSqlKey openDayGarage)], [cancelButton langs]])

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
    (toStrict $ renderHtml $ defaultLayout langs [ihamlet|#{clock} _{MsgChooseStartTime}#{mySlots}|])
    (timeGrid day Nothing <> [[ikb (calendar <> " " <> tr langs MsgChangeDay) ("garage_" <> showSqlKey openDayGarage)], [cancelButton langs]])

endTimeStep ::
  [Lang] ->
  ConnectionPool ->
  ChatChannel ->
  Maybe MessageId ->
  OpenDayId ->
  Text ->
  ClientM MessageId
endTimeStep langs _ chat@ChatChannel {..} msgId day startTime = do
  menuStep
    chat
    msgId
    (clock <> " " <> tr langs MsgChooseEndTime)
    (timeGrid day (Just startTime) <> [[ikb (clock <> " " <> tr langs MsgChangeStartTime) ("day_" <> showSqlKey day)], [cancelButton langs]])

-- Intervals interesect iff the start of either interval lies within the other interval
timesIntersect :: Ord a => (a, a) -> (a, a) -> Bool
timesIntersect (sa, ea) (sb, eb) =
  sa <= sb && sb < ea -- Start of b lies within a
    || sb <= sa && sa < eb -- Start of a lies within b

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
  conflicts <-
    runInPool pool $ do
      Just OpenDay {openDayDate} <- get day
      days <- selectList [OpenDayDate ==. openDayDate] []
      conflicts <- forM days $ \(Entity openDay _) ->
        selectList [ScheduledSlotDay ==. openDay, ScheduledSlotUser ==. volunteer] []
          <&> filter (\(Entity _ ScheduledSlot {..}) -> timesIntersect (scheduledSlotStartTime, scheduledSlotEndTime) (startTime, endTime))
      mapM (getSlotDesc langs . entityVal) (concat conflicts)
  others <- runInPool pool $ othersSlots day volunteer
  let othersMessage =
        [ihamlet|
    $if not (null others)
      \
      \
      #{people} _{MsgOtherVolunteers}
      $forall (Entity _ ScheduledSlot {scheduledSlotStartTime, scheduledSlotEndTime}, v) <- others
        \
        #{v}: #{showHourMinutes scheduledSlotStartTime}—#{showHourMinutes scheduledSlotEndTime}
    $else
  |]
  Just OpenDay {openDayGarage} <- runInPool pool $ get day
  mySlots <- mySlotsMsg langs pool day volunteer
  let extraButtons =
        [ [ikb (clock <> " " <> tr langs MsgChangeStartTime) ("day_" <> showSqlKey day)],
          [ikb (calendar <> " " <> tr langs MsgChangeDay) ("garage_" <> showSqlKey openDayGarage)],
          [ikb (house <> " " <> tr langs MsgChangeGarage) "signup"],
          [cancelButton langs]
        ]
  slotDesc <-
    runInPool pool $
      getSlotDesc
        langs
        ( ScheduledSlot
            { scheduledSlotDay = day,
              scheduledSlotStartTime = startTime,
              scheduledSlotEndTime = endTime,
              scheduledSlotState = ScheduledSlotCreated,
              scheduledSlotReminderSent = False,
              scheduledSlotUser = volunteer
            }
        )
  if not $ null conflicts
    then do
      menuStep
        chat
        msgId
        ( toStrict $
            renderHtml $
              defaultLayout
                langs
                [ihamlet|
          #{forbidden} _{MsgOtherDuties}
          \
          #{slotDesc}
          \
          #{diamond} _{MsgConflicts}
          $forall duty <- conflicts
            \
            \
            #{duty}
          ^{othersMessage}
        |]
        )
        ([[ikb (attention <> " " <> tr langs MsgCancelCreate) ("create_cancel_" <> showSqlKey day <> "_" <> showHourMinutes startTime <> "_" <> showHourMinutes endTime)]] <> extraButtons)
    else do
      menuStep
        chat
        msgId
        ( toStrict $
            renderHtml $
              defaultLayout
                langs
                [ihamlet|
          _{MsgCreate}
          #{slotDesc}#{mySlots}^{othersMessage}
        |]
        )
        ([[ikb (allGood <> " " <> tr langs MsgYesCreate) ("create_" <> showSqlKey day <> "_" <> showHourMinutes startTime <> "_" <> showHourMinutes endTime)]] <> extraButtons)

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
  slotId <-
    runInPool
      pool
      ( insert
          ( ScheduledSlot
              { scheduledSlotDay = day,
                scheduledSlotStartTime = startTime,
                scheduledSlotEndTime = endTime,
                scheduledSlotUser = volunteer,
                scheduledSlotState = ScheduledSlotCreated,
                scheduledSlotReminderSent = False
              }
          )
      )
  Just slot <- runInPool pool $ get slotId
  slotDesc <- runInPool pool $ getSlotDesc langs slot
  Just OpenDay {openDayGarage, openDayDate} <- runInPool pool $ get (scheduledSlotDay slot)
  void $
    menuStep
      chat
      msgId
      ( defaultRender
          langs
          [ihamlet|
        #{allGood} _{MsgCreated}
        #{slotDesc}
        |]
      )
      [[cancelSlotButton langs slotId]]
  menuStep chat Nothing (tr langs MsgAnotherSlot) [[ikb (tr langs MsgAnotherGarage) "signup"], [ikb (tr langs MsgSameGarage) ("garage_" <> showSqlKey openDayGarage)], [ikb (tr langs MsgDone) "cancel"]]
    <* updateWorkingSchedule pool False (thisWeekStart openDayDate) openDayGarage
    `catchError` (liftIO . print)

list :: [Lang] -> ChatChannel -> VolunteerId -> ConnectionPool -> ClientM ()
list langs ChatChannel {channelChatId} volunteer pool = do
  today <- localDay . zonedTimeToLocalTime <$> liftIO getZonedTime
  slots <-
    runInPool pool $ do
      days <- fmap entityKey <$> selectList [OpenDayDate >=. today] []
      selectList [ScheduledSlotUser ==. volunteer, ScheduledSlotDay <-. days, ScheduledSlotState <-. [ScheduledSlotCreated, ScheduledSlotAwaitingConfirmation True, ScheduledSlotAwaitingConfirmation False, ScheduledSlotConfirmed, ScheduledSlotUnconfirmed]] []
  when (null slots) $ void $ send channelChatId langs MsgNoSlots
  forM_ slots $ \(Entity slotId slot) -> do
    slotDesc <- runInPool pool $ getSlotDesc langs slot
    sendMessage
      (sendMessageRequest channelChatId (defaultRender langs [ihamlet|#{slotDesc}|]))
        { sendMessageParseMode = Just HTML,
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
      (sendMessageRequest channelChatId (attention <> " " <> tr langs MsgAreYouSure))
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
  flip catchError (liftIO . print) $
    void $
      sendMessage
        ( sendMessageRequest
            (ChatId (fromIntegral telegramUserUserId))
            ( defaultRender
                langs
                [ihamlet|
            #{attention} _{MsgYourSlotCancelled}
            #{slotDesc}
          |]
            )
        )
          { sendMessageParseMode = Just HTML
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
              ( defaultRender
                  adminLangs
                  [ihamlet|
                #{attention} _{MsgSlotCancelled}
                #{slotFullDesc}
              |]
              )
          )
            { sendMessageParseMode = Just HTML,
              sendMessageReplyMarkup =
                Just (ik [[ikb (tr adminLangs MsgLock) ("admin_lock_" <> pack (showGregorian weekStart))]])
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
            ( defaultRender
                langs
                [ihamlet|
              #{attention} _{MsgSureCancel}
              #{slotDesc}
            |]
            )
        )
          { sendMessageParseMode = Just HTML,
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
    flip catchError (liftIO . print) $ void $ deleteMessage channelChatId originalMsgId
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
  updateWorkingScheduleForDay pool False (scheduledSlotDay slot)
  void $
    sendMessage
      ( sendMessageRequest
          channelChatId
          -- (allGood <> tr langs MsgConfirmedShort <> "\n" <> slotDesc)
          ( defaultRender
              langs
              [ihamlet|
            #{allGood} _{MsgConfirmedShort}
            #{slotDesc}
          |]
          )
      )
        { sendMessageParseMode = Just HTML,
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
              ( defaultRender
                  langs
                  [ihamlet|
              #{hello} _{MsgVolunteerRequest $ renderUser user}
              |]
              )
          )
            { sendMessageParseMode = Just HTML,
              sendMessageReplyMarkup =
                Just $
                  ik
                    [ [ ikb
                          (allGood <> " " <> tr langs MsgAllow)
                          ("admin_allow_" <> showSqlKey tuid),
                        ikb
                          (bad <> " " <> tr langs MsgDecline)
                          ("admin_decline_" <> showSqlKey tuid)
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
              ( defaultRender
                  langs
                  [ihamlet|
                #{party} _{MsgNewVolunteer $ renderUser user}
              |]
              )
          )
            { sendMessageParseMode = Just HTML,
              sendMessageReplyMarkup =
                Just $
                  ik
                    [ [ ikb
                          (forbidden <> " " <> tr langs MsgBan)
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
        { sendMessageParseMode = Just HTML
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
        { sendMessageParseMode = Just HTML
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

renderState :: ScheduledSlotState -> Text
renderState ScheduledSlotCreated = news
renderState (ScheduledSlotAwaitingConfirmation _) = hourglass
renderState ScheduledSlotUnconfirmed = attention
renderState ScheduledSlotConfirmed = allGood
renderState ScheduledSlotFinished {} = finished
renderState ScheduledSlotChecklistComplete {visitors} = finished <> " (" <> pack (show visitors) <> " " <> people <> ")"

type ScheduleList = [(Day, [(TelegramUser, TimeOfDay, TimeOfDay, ScheduledSlotState)])]

renderWorkingSchedule :: [Lang] -> Garage -> ScheduleList -> Text
renderWorkingSchedule langs garage days =
  defaultRender
    langs
    [ihamlet|
    #{calendar} <b>_{MsgWorkingScheduleFor $ renderGarage garage}</b>
    $forall (day, slots) <- days
      \
      \
      <b>#{showDay langs day}
      $forall (user, start, end, state) <- slots
        \
        #{clock} #{renderUser user}: #{showHourMinutes start}-#{showHourMinutes end}: #{renderState state}
    \
    \
    _{MsgLegend}: <span class="tg-spoiler">#{news}_{MsgLegendCreated} <b>/</b> #{hourglass}_{MsgLegendAwaitingConfirmation} <b>/</b> #{attention}_{MsgLegendUnconfirmed} <b>/</b> #{allGood}_{MsgLegendConfirmed} <b>/</b> #{finished}_{MsgLegendFinished} <b>/</b> #{people}_{MsgLegendVisitors}</span>
  |]

getWorkingSchedule :: ConnectionPool -> Day -> GarageId -> ClientM ScheduleList
getWorkingSchedule pool weekStart garage = do
  Just Garage {..} <- runInPool pool $ get garage
  let selector = [OpenDayGarage ==. garage, OpenDayDate <-. take 7 [weekStart ..]]
  runInPool pool $
    selectList selector []
      >>= mapM
        ( \(Entity day OpenDay {..}) -> do
            slots <- runInPool pool $ selectList [ScheduledSlotDay ==. day] []
            times <- forM slots $ \(Entity _ ScheduledSlot {..}) -> do
              Just Volunteer {..} <- get scheduledSlotUser
              Just u <- get volunteerUser
              pure (u, scheduledSlotStartTime, scheduledSlotEndTime, scheduledSlotState)
            pure (openDayDate, times)
        )

updateWorkingSchedule :: ConnectionPool -> Bool -> Day -> GarageId -> ClientM ()
updateWorkingSchedule pool recreate weekStart garage = do
  admins <- runInPool pool (selectList [] [] >>= mapM (get . adminUser . entityVal))
  Just g <- runInPool pool $ get garage
  let s = "working_schedule_" <> showSqlKey garage <> "_" <> pack (showGregorian weekStart)
  forM_ admins $ \(Just (TelegramUser auid lang _ _)) -> do
    liftIO $ threadDelay 100000 -- Telegram rate limits
    let langs = maybeToList lang
    t <- renderWorkingSchedule langs g <$> getWorkingSchedule pool weekStart garage
    messages <- runInPool pool $ selectList [CallbackQueryMultiChatCallbackQuery ==. s, CallbackQueryMultiChatChatId ==. auid] []
    flip catchError (liftIO . print) $ case (recreate, messages) of
      (False, [Entity _ CallbackQueryMultiChat {..}]) -> do
        flip catchError (liftIO . print) $
          void $
            editMessageText
              (editMessageTextRequest t)
                { editMessageTextMessageId = Just (MessageId $ fromIntegral callbackQueryMultiChatMsgId),
                  editMessageTextChatId = Just $ SomeChatId (ChatId $ fromIntegral auid),
                  editMessageTextParseMode = Just HTML,
                  editMessageTextReplyMarkup = Just $ ik [[ikb (tr langs MsgLock) ("admin_lock_" <> showSqlKey garage <> "_" <> pack (showGregorian weekStart))]]
                }
      (_, msgs) -> do
        MessageId mid <-
          messageMessageId . responseResult
            <$> sendMessage
              (sendMessageRequest (ChatId (fromIntegral auid)) t)
                { sendMessageParseMode = Just HTML,
                  sendMessageReplyMarkup = Just $ ik [[ikb (tr langs MsgLock) ("admin_lock_" <> showSqlKey garage <> "_" <> pack (showGregorian weekStart))]]
                }
        void $ runInPool pool $ insert $ CallbackQueryMultiChat auid (fromIntegral mid) s
        forM_ msgs $ \(Entity _ CallbackQueryMultiChat {..}) -> do
          flip catchError (liftIO . print) $ void $ deleteMessage (ChatId (fromIntegral auid)) (MessageId $ fromIntegral callbackQueryMultiChatMsgId)
          runInPool pool $ deleteWhere [CallbackQueryMultiChatChatId ==. auid, CallbackQueryMultiChatMsgId ==. callbackQueryMultiChatMsgId]

updateWorkingScheduleForDay :: ConnectionPool -> Bool -> OpenDayId -> ClientM ()
updateWorkingScheduleForDay pool recreate dayId = do
  (weekStart, gid) <-
    runInPool pool $ do
      Just OpenDay {..} <- get dayId
      let weekStart = thisWeekStart openDayDate
      pure (weekStart, openDayGarage)
  updateWorkingSchedule pool recreate weekStart gid

sendOpenDaySchedule :: ConnectionPool -> Day -> GarageId -> [Entity OpenDay] -> ClientM ()
sendOpenDaySchedule pool weekStart garage days = do
  subs <- runInPool pool (selectList [] [] >>= mapM (get . subscriptionUser . entityVal))
  Just g@Garage {..} <- runInPool pool $ get garage
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
                      <> " "
                      <> tr langs (MsgNewScheduleFor $ renderGarage g)
                  )
              )
                { sendMessageParseMode = Just HTML,
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
          void $ runInPool pool $ insert $ CallbackQueryMultiChat uid (fromIntegral mid) ("schedule_" <> showSqlKey garage <> "_" <> pack (showGregorian weekStart))
      )
      (liftIO . hPrint stderr)

makeSchedule :: [Lang] -> ConnectionPool -> ChatChannel -> Maybe Day -> GarageId -> ClientM ()
makeSchedule langs pool chat day' gid = do
  day <- liftIO $ case day' of
    Just d -> pure d
    Nothing -> nextWeekStart . localDay . zonedTimeToLocalTime <$> getZonedTime
  Just g@Garage {..} <- runInPool pool $ get gid
  let nextWeek = take 7 [day ..]
  defaultDays <-
    fmap (defaultOpenDayDayOfWeek . entityVal)
      <$> runInPool pool (selectList [DefaultOpenDayGarage ==. gid] [])
  let defaultDays' =
        pack . showGregorian
          <$> Prelude.filter ((`elem` defaultDays) . dayOfWeek) nextWeek
  inlinePoll
    chat
    (const True)
    (tr langs (MsgGarageOpenDays $ renderGarage g))
    [[(showDay langs d, pack $ showGregorian d)] | d <- nextWeek]
    defaultDays'
    >>= ( \case
            Nothing -> pure ()
            Just days -> do
              updatedDays <- runInPool pool $ do
                deleteWhere [DefaultOpenDayGarage ==. gid]
                mapM_ (insert . DefaultOpenDay gid) (fmap dayOfWeek days)
                deleteWhere [OpenDayGarage ==. gid, OpenDayDate <-. (take 7 [day ..] \\ days)]
                mapM
                  ( \updatedDay ->
                      upsertBy
                        (UniqueDay updatedDay gid)
                        (OpenDay gid updatedDay True)
                        [OpenDayAvailable =. True]
                  )
                  days
              sendOpenDaySchedule pool day gid updatedDays
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
    "\n\n"
    [ "<b>"
        <> showDay langs day
        <> "</b>"
        <> "\n\n"
        <> intercalate
          "\n"
          [showHourMinutes start <> "-" <> showHourMinutes end | (start, end) <- mergeIntervals times]
      | (day, times) <- L.sort s,
        times /= []
    ]

lockSchedule :: [Lang] -> ConnectionPool -> ChatChannel -> Maybe Day -> GarageId -> ClientM ()
lockSchedule langs pool ChatChannel {..} day' gid = do
  day <- liftIO $ case day' of
    Just d -> pure d
    Nothing -> nextWeekStart . localDay . zonedTimeToLocalTime <$> getZonedTime
  Just g@Garage {..} <- runInPool pool $ get gid
  deleteCallbackQueryMessages pool ("schedule_" <> showSqlKey gid <> "_" <> pack (showGregorian day))
  void $
    sendMessage
      (sendMessageRequest channelChatId (tr langs (MsgLocked $ renderGarage g)))
        { sendMessageParseMode = Just HTML,
          sendMessageReplyMarkup =
            Just $ ik [[ikb (tr langs MsgUnlock) ("admin_unlock_" <> showSqlKey gid <> "_" <> pack (showGregorian day))]]
        }
  let nextWeek = take 7 [day ..]
  let selector = [OpenDayGarage ==. gid, OpenDayDate <-. nextWeek]
  openDays <-
    runInPool pool $ do
      updateWhere selector [OpenDayAvailable =. False]
      selectList selector []
  openDaysWithSlots <-
    forM openDays $ \(Entity openDay OpenDay {..}) -> do
      slots <- runInPool pool $ selectList [ScheduledSlotDay ==. openDay] []
      pure
        ( openDayDate,
          [ (scheduledSlotStartTime, scheduledSlotEndTime)
            | Entity _ ScheduledSlot {..} <- slots
          ]
        )
  subscriptions <- runInPool pool getSubscriptions
  forM_ subscriptions $ \(TelegramUser suid lang _ _) -> do
    let subscriptionLangs = maybeToList lang
    void $
      sendMessage
        ( sendMessageRequest
            (ChatId (fromIntegral suid))
            ( tr subscriptionLangs (MsgScheduleIntro $ renderGarage g)
                <> "\n\n"
                <> renderSchedule subscriptionLangs openDaysWithSlots
            )
        )
          { sendMessageParseMode = Just HTML
          }
    liftIO $ threadDelay 500000

unlockSchedule :: [Lang] -> ConnectionPool -> ChatChannel -> Maybe Day -> GarageId -> ClientM ()
unlockSchedule langs pool ChatChannel {..} day' gid = do
  day <- liftIO $ case day' of
    Just d -> pure d
    Nothing -> nextWeekStart . localDay . zonedTimeToLocalTime <$> getZonedTime
  Just g@Garage {..} <- runInPool pool $ get gid
  void $
    sendMessage
      (sendMessageRequest channelChatId (tr langs (MsgUnlocked $ renderGarage g)))
        { sendMessageParseMode = Just HTML
        }
  let nextWeek = take 7 [day ..]
  let selector = [OpenDayGarage ==. gid, OpenDayDate <-. nextWeek]
  days <- runInPool pool (updateWhere selector [OpenDayAvailable =. True] >> selectList selector [])
  sendOpenDaySchedule pool day gid days

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
  [ ("setopendays", MsgCommandSetOpenDays),
    ("lock", MsgCommandLock),
    ("workingschedule", MsgCommandWorkingSchedule),
    ("workingschedulethisweek", MsgCommandWorkingScheduleThisWeek)
  ]

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

untilTrue :: Monad m => [m Bool] -> m ()
untilTrue [] = pure ()
untilTrue (a : as) =
  a >>= \case
    True -> pure ()
    False -> untilTrue as

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

      let adminHandler = \case
            SomeNewMessage Message {..} ->
              case messageText of
                Just "/start" ->
                  sendMessage
                    (sendMessageRequest channelChatId "Welcome, admin!")
                    $> True
                Just "/setopendays" -> (runInPool pool (selectList [] []) >>= mapM_ (makeSchedule langs pool chat Nothing . entityKey)) $> True
                Just "/lock" -> (runInPool pool (selectList [] []) >>= mapM_ (lockSchedule langs pool chat Nothing . entityKey)) $> True
                Just "/workingschedule" -> do
                  today <- localDay . zonedTimeToLocalTime <$> liftIO getZonedTime
                  runInPool pool (selectList [] []) >>= mapM_ (updateWorkingSchedule pool True (nextWeekStart today) . entityKey)
                  pure True
                Just "/workingschedulethisweek" -> do
                  today <- localDay . zonedTimeToLocalTime <$> liftIO getZonedTime
                  runInPool pool (selectList [] []) >>= mapM_ (updateWorkingSchedule pool True (thisWeekStart today) . entityKey)
                  pure True
                ((>>= stripPrefix "/cancel_") -> Just t) -> cancelSlot pool (readSqlKey t) $> True
                _ -> pure False
            SomeNewCallbackQuery
              CallbackQuery
                { callbackQueryData = Just txt,
                  callbackQueryMessage = Just Message {..},
                  callbackQueryId
                } -> do
                void $
                  answerCallbackQuery (answerCallbackQueryRequest callbackQueryId)
                case T.splitOn "_" txt of
                  ["admin", "allow", t] -> do
                    allowVolunteer pool $ readSqlKey t
                    void $ deleteMessage channelChatId messageMessageId
                    pure True
                  ["admin", "decline", _] -> do
                    void $ deleteMessage channelChatId messageMessageId
                    pure True
                  ["admin", "ban", t] -> do
                    banVolunteer pool $ read $ unpack t
                    pure True
                  ["admin", "cancel", t] -> do
                    void $ deleteMessage channelChatId messageMessageId
                    cancelSlot pool $ readSqlKey t
                    pure True
                  ["admin", "setopendays", g, d] -> makeSchedule langs pool chat (parseGregorian d) (readSqlKey g) $> True
                  ["admin", "lock", g, d] -> lockSchedule langs pool chat (parseGregorian d) (readSqlKey g) $> True
                  ["admin", "unlock", g, d] -> unlockSchedule langs pool chat (parseGregorian d) (readSqlKey g) $> True
                  _ -> pure False
            _ -> pure False

      let volunteerHandler volunteer = \case
            SomeNewMessage m@Message {..} ->
              case messageText of
                Just "/start" -> reply m langs MsgVolunteer $> True
                Just "/signup" -> garageStep langs pool chat Nothing $> True
                Just "/list" -> list langs chat volunteer pool $> True
                Just "/subscribe" -> subscribe langs chat dbUserId pool $> True
                Just "/unsubscribe" -> unsubscribe langs chat dbUserId pool $> True
                Just "/delete" -> askDeleteUser langs chat volunteer pool $> True
                Just text -> case messageReplyToMessage of
                  Just (Message {messageMessageId = MessageId mid}) ->
                    runInPool pool (selectFirst [ScheduledSlotState ==. ScheduledSlotFinished (fromIntegral mid)] []) >>= \case
                      Just (Entity slotId ScheduledSlot {..}) -> case readMay (unpack text) of
                        Just visitors -> do
                          runInPool pool $ update slotId [ScheduledSlotState =. ScheduledSlotChecklistComplete {visitors}]
                          updateWorkingScheduleForDay pool False scheduledSlotDay
                          void $ reply m langs MsgAck
                          pure True
                        Nothing -> pure False
                      Nothing -> pure False
                  Nothing -> pure False
                _ -> pure False
            SomeNewCallbackQuery
              CallbackQuery
                { callbackQueryData = Just txt,
                  callbackQueryMessage = Just Message {..},
                  callbackQueryId
                } -> do
                void $
                  answerCallbackQuery
                    (answerCallbackQueryRequest callbackQueryId)
                case T.splitOn "_" txt of
                  ["signup", t] -> do
                    let day = readSqlKey t
                    void $ existingSlotsStep langs pool chat Nothing volunteer day
                    pure True
                  ["cancel"] -> deleteMessage channelChatId messageMessageId $> True
                  ["signup"] -> garageStep langs pool chat (Just messageMessageId) $> True
                  ["garage", t] -> dayStep langs pool chat (Just messageMessageId) volunteer (readSqlKey t) $> True
                  ["day", t] -> existingSlotsStep langs pool chat (Just messageMessageId) volunteer (readSqlKey t) $> True
                  ["new", t] -> startTimeStep langs pool chat (Just messageMessageId) volunteer (readSqlKey t) $> True
                  ["start", d, s] -> endTimeStep langs pool chat (Just messageMessageId) (readSqlKey d) s $> True
                  ["end", d, s, e] -> askCreateStep langs pool chat (Just messageMessageId) volunteer (readSqlKey d) (fromJust $ parseHourMinutesM s) (fromJust $ parseHourMinutesM e) $> True
                  ["create", "cancel", d, s, e] -> do
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
                    pure True
                  ["create", d, s, e] -> slotCreated langs pool chat (Just messageMessageId) volunteer (readSqlKey d) (fromJust $ parseHourMinutesM s) (fromJust $ parseHourMinutesM e) $> True
                  ["cancel", t] ->
                    askCancelSlot langs pool chat messageMessageId (readSqlKey t) $> True
                  ["confirm", d] ->
                    confirmSlot langs pool chat messageMessageId (readSqlKey d) $> True
                  _ -> pure False
            _ -> pure False

      let newUserHandler = \case
            SomeNewMessage Message {..} ->
              case messageText of
                Just "/start" ->
                  askForPermission pool u
                    >> void (send channelChatId langs MsgGreet)
                      $> True
                _ -> pure False
            _ -> pure False

      let failHandler = \case SomeNewMessage m -> reply m langs MsgNotRecognized $> True; _ -> pure True

      -- Run handlers until one of them returns true, or notify that the message is not recognized
      let runHandlers h = untilTrue (h ++ [failHandler upd])

      runInPool pool ((,) <$> getBy (UniqueAdmin dbUserId) <*> getBy (UniqueVolunteer dbUserId)) >>= \case
        (Just (Entity _ _), Just (Entity volunteer _)) -> do
          setCommands (adminCommands ++ volunteerCommands) chat
          runHandlers [volunteerHandler volunteer upd, adminHandler upd]
        (Just (Entity _ _), Nothing) -> do
          setCommands adminCommands chat
          runHandlers [adminHandler upd]
        (Nothing, Just (Entity volunteer _)) -> do
          setCommands volunteerCommands chat
          runHandlers [volunteerHandler volunteer upd]
        (Nothing, Nothing) ->
          runHandlers [newUserHandler upd]
    _ ->
      liftIO $ hPutStrLn stderr "Event from a non-private chat; Doing nothing"
  liftIO $ threadDelay 1500000