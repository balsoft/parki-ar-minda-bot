{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-record-wildcards #-}

module Bot
  ( bot,
  )
where

import Control.Concurrent (threadDelay)
import Control.Monad (forM, forM_, forever, unless, void, when, (>=>))
import Control.Monad.Except (MonadError (catchError))
import Control.Monad.IO.Unlift (MonadIO (liftIO))
import Control.Monad.Reader (ReaderT)
import qualified Data.ByteString.Lazy as BS
import Data.Csv
import Data.Functor (($>), (<&>))
import Data.List ((\\))
import qualified Data.List as L
import Data.Maybe (fromJust, isJust, mapMaybe, maybeToList)
import Data.Text as T (Text, pack, replace, splitOn, stripPrefix, unpack)
import Data.Text.IO (hPutStrLn)
import Data.Time
import Database.Persist
import Database.Persist.Sqlite (ConnectionPool, SqlBackend)
import I18N
import Persist
import Safe (readMay)
import Servant.Client hiding (Response)
import Symbols
import System.Directory (removeFile)
import System.IO (hPrint, stderr)
import System.IO.Temp
import Telegram.Bot.API
import Telegram.Bot.Monadic
import Text.Hamlet (ihamlet, ihamletFile)
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
  [[(IHamlet, Text)]]
timeGrid d s =
  fmap reverse $
    reverse $
      chunksOf 2 $
        reverse
          (fmap (\t -> (__ $ showHourMinutes t, label <> showSqlKey d <> "_" <> optionalStartTime <> showHourMinutes t)) times)
  where
    (timeLessThan, timeNoMoreThan) =
      case parseHourMinutesM =<< s of
        Just (TimeOfDay sHour sMinute _) ->
          ( (TimeOfDay (sHour + 1) sMinute 0 >),
            (TimeOfDay (sHour + 3) sMinute 0 >=)
          )
        Nothing -> (const False, const True)
    lastHour = if isJust s then 22 else 21
    times =
      L.takeWhile timeNoMoreThan $
        dropWhile
          timeLessThan
          [TimeOfDay hour minute 0 | hour <- [9 .. lastHour], minute <- [0, 30], hour /= lastHour || minute /= 30]
    label = if isJust s then "end_" else "start_"
    optionalStartTime = maybe "" (<> "_") s

cancelButton :: (IHamlet, Text)
cancelButton = (__ MsgCancel, "cancel")

menuStep ::
  [Lang] -> ChatChannel -> Maybe MessageId -> IHamlet -> [[(IHamlet, Text)]] -> ClientM MessageId
menuStep langs ChatChannel {..} Nothing msg grid =
  messageMessageId . responseResult
    <$> sendWithButtons channelChatId langs msg grid
menuStep langs ChatChannel {..} (Just msgId) msg grid =
  catchError
    ( void $
        editWithButtons channelChatId langs msgId msg grid
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
        [ [(__ garageName, "garage_" <> showSqlKey gid)]
          | Entity gid (Garage {..}) <- garages
        ]
  menuStep langs chat msgId [ihamlet|#{house} _{MsgChooseGarage}|] (grid <> [[cancelButton]])

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
  let anotherGarage = [[([ihamlet|#{house} _{MsgChangeGarage}|], "signup")]]
  if null days
    then do
      menuStep langs chat msgId [ihamlet|#{forbidden} _{MsgNoDays}|] (anotherGarage <> [[cancelButton]])
    else do
      Just g <- runInPool pool $ get garage
      let weekStart = thisWeekStart (openDayDate $ entityVal $ (\(a, _, _) -> a) $ head days)
      schedule <- renderWorkingSchedule g False False <$> getWorkingSchedule pool weekStart garage <*> pure []
      let grid =
            [ [ ( [ihamlet|
                  _{showDate openDayDate}
                  $if otherPeople
                     #{people}
                  $if me
                     #{diamond}
                  |],
                  "day_" <> showSqlKey did
                )
              ]
              | (Entity did (OpenDay {..}), otherPeople, me) <- days
            ]
      menuStep
        langs
        chat
        msgId
        [ihamlet|
          #{calendar} _{MsgChooseDay}
          \
          ^{schedule}
        |]
        (grid <> anotherGarage <> [[cancelButton]])

getMySlots :: (MonadIO m, MonadFail m) => Key OpenDay -> VolunteerId -> ReaderT SqlBackend m [IHamlet]
getMySlots day volunteer = do
  Just OpenDay {..} <- get day
  mapM (getSlotDesc . entityVal) . concat
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
  ConnectionPool ->
  Key OpenDay ->
  VolunteerId ->
  m IHamlet
mySlotsMsg pool day volunteer = runInPool pool $ do
  mySlots <- getMySlots day volunteer
  pure $(ihamletFile "templates/my_slots.ihamlet")

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
  let anotherDay = [[([ihamlet|#{calendar} _{MsgChangeDay}|], "garage_" <> showSqlKey openDayGarage)]]
  if not openDayAvailable
    then do
      menuStep langs chat msgId [ihamlet|#{forbidden} _{MsgDayUnavailable}|] (anotherDay <> [[cancelButton]])
    else do
      slots <- runInPool pool $ othersSlots day volunteer
      if null slots
        then startTimeStep langs pool chat msgId volunteer day
        else do
          mySlots <- mySlotsMsg pool day volunteer
          let grid =
                [ [ ( [ihamlet|#{person} #{name}: #{showHourMinutes scheduledSlotStartTime}—#{showHourMinutes scheduledSlotEndTime}|],
                      "end_" <> showSqlKey scheduledSlotDay <> "_" <> showHourMinutes scheduledSlotStartTime <> "_" <> showHourMinutes scheduledSlotEndTime
                    )
                  ]
                  | (Entity _ ScheduledSlot {..}, name) <- slots
                ]
                  ++ [ [ ( [ihamlet|#{new} _{MsgNewAppointment}|],
                           "new_" <> showSqlKey day
                         )
                       ]
                     ]
          menuStep
            langs
            chat
            msgId
            [ihamlet|#{people} _{MsgOtherVolunteers} _{MsgChooseExistingSlot}^{mySlots}|]
            (grid <> [[([ihamlet|#{calendar} _{MsgChangeDay}|], "garage_" <> showSqlKey openDayGarage)], [cancelButton]])

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
  mySlots <- mySlotsMsg pool day volunteer
  menuStep
    langs
    chat
    msgId
    [ihamlet|
      #{clock} _{MsgChooseStartTime}
      ^{mySlots}
    |]
    (timeGrid day Nothing <> [[([ihamlet|#{calendar} _{MsgChangeDay}|], "garage_" <> showSqlKey openDayGarage)], [cancelButton]])

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
    langs
    chat
    msgId
    [ihamlet|#{clock} _{MsgChooseEndTime}|]
    (timeGrid day (Just startTime) <> [[([ihamlet|#{clock} _{MsgChangeStartTime}|], "day_" <> showSqlKey day)], [cancelButton]])

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
      mapM (getSlotDesc . entityVal) (concat conflicts)
  others <- runInPool pool $ othersSlots day volunteer
  let othersMessage = $(ihamletFile "templates/other_volunteers.ihamlet")
  Just OpenDay {openDayGarage} <- runInPool pool $ get day
  mySlots <- mySlotsMsg pool day volunteer
  let extraButtons =
        [ [([ihamlet|#{clock} _{MsgChangeStartTime}|], "day_" <> showSqlKey day)],
          [([ihamlet|#{calendar} _{MsgChangeDay}|], "garage_" <> showSqlKey openDayGarage)],
          [([ihamlet|#{house} _{MsgChangeGarage}|], "signup")],
          [cancelButton]
        ]
  slotDesc <-
    runInPool pool $
      getSlotDesc
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
        langs
        chat
        msgId
        [ihamlet|
          #{forbidden} _{MsgOtherDuties}
          \
          ^{slotDesc}
          \
          #{diamond} _{MsgConflicts}
          $forall duty <- conflicts
            \
            \
            ^{duty}
          ^{othersMessage}
        |]
        ([[([ihamlet|#{attention} _{MsgCancelCreate}|], "create_cancel_" <> showSqlKey day <> "_" <> showHourMinutes startTime <> "_" <> showHourMinutes endTime)]] <> extraButtons)
    else do
      menuStep
        langs
        chat
        msgId
        [ihamlet|
          _{MsgCreate}
          ^{slotDesc}^{mySlots}^{othersMessage}
        |]
        ([[([ihamlet|#{allGood} _{MsgYesCreate}|], "create_" <> showSqlKey day <> "_" <> showHourMinutes startTime <> "_" <> showHourMinutes endTime)]] <> extraButtons)

cancelSlotButton :: ScheduledSlotId -> (IHamlet, Text)
cancelSlotButton slotId =
  (__ MsgCantCome, "cancel_" <> showSqlKey slotId)

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
  slotDesc <- runInPool pool $ getSlotDesc slot
  Just OpenDay {openDayGarage, openDayDate} <- runInPool pool $ get (scheduledSlotDay slot)
  void $
    menuStep
      langs
      chat
      msgId
      [ihamlet|
        #{allGood} _{MsgCreated}
        ^{slotDesc}
      |]
      [[cancelSlotButton slotId]]
  menuStep langs chat Nothing (__ MsgAnotherSlot) [[(__ MsgAnotherGarage, "signup")], [(__ MsgSameGarage, "garage_" <> showSqlKey openDayGarage)], [(__ MsgDone, "cancel")]]
    <* updateWorkingSchedule pool False (thisWeekStart openDayDate) openDayGarage
    `catchError` (liftIO . hPrint stderr)

list :: [Lang] -> ChatChannel -> VolunteerId -> ConnectionPool -> ClientM ()
list langs ChatChannel {channelChatId} volunteer pool = do
  today <- localDay . zonedTimeToLocalTime <$> liftIO getZonedTime
  slots <-
    runInPool pool $ do
      days <- fmap entityKey <$> selectList [OpenDayDate >=. today] []
      selectList [ScheduledSlotUser ==. volunteer, ScheduledSlotDay <-. days, ScheduledSlotState <-. [ScheduledSlotCreated, ScheduledSlotAwaitingConfirmation True, ScheduledSlotAwaitingConfirmation False, ScheduledSlotConfirmed, ScheduledSlotUnconfirmed]] []
  when (null slots) $ void $ send channelChatId langs $ [ihamlet|#{info} _{MsgNoSlots}|]
  forM_ slots $ \(Entity slotId slot) -> do
    slotDesc <- runInPool pool $ getSlotDesc slot
    sendWithButtons channelChatId langs slotDesc [[cancelSlotButton slotId]]

subscribe ::
  [Lang] -> ChatChannel -> TelegramUserId -> ConnectionPool -> ClientM ()
subscribe langs ChatChannel {channelChatId} uid pool = do
  void $ runInPool pool $ upsert (Subscription uid) []
  void $ send channelChatId langs [ihamlet|#{allGood} _{MsgSubscribed}|]

unsubscribe ::
  [Lang] -> ChatChannel -> TelegramUserId -> ConnectionPool -> ClientM ()
unsubscribe langs ChatChannel {channelChatId} uid pool = do
  void $ runInPool pool $ deleteBy $ UniqueSubscription uid
  void $ send channelChatId langs [ihamlet|#{allGood} _{MsgUnsubscribed}|]

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
  Just Volunteer {volunteerUser} <- runInPool pool $ get volunteer
  Just TelegramUser {telegramUserUserId} <- runInPool pool $ get volunteerUser
  when (channelChatId == ChatId (fromIntegral telegramUserUserId)) $ do
    void $
      send channelChatId langs (attention +-+ MsgAreYouSure)
    untilRight (getNewMessage chat) (const $ pure ()) >>= \case
      Message {messageText = Just txt}
        | txt == MsgIAmSure |-> langs -> do
            deleteUser pool volunteer
            void $ send channelChatId langs (allGood +-+ MsgDeleted)
      _ -> void $ send channelChatId langs (attention +-+ MsgNotDeleting)

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
      desc <- getSlotDesc slot
      pure (slot, desc, openDayAvailable, langs, user, openDayGarage, weekStart)
  runInPool pool $ delete slotId
  Just me <- userUsername . responseResult <$> getMe
  let slotDay = showSqlKey (scheduledSlotDay slot)
  let startTime = T.replace ":" "-" $ showHourMinutes (scheduledSlotStartTime slot)
  let endTime = T.replace ":" "-" $ showHourMinutes (scheduledSlotEndTime slot)
  let link = [ihamlet|https://t.me/#{me}?start=#{slotDay}_#{startTime}_#{endTime}|]
  flip catchError (liftIO . hPrint stderr) $
    void $
      send
        (ChatId (fromIntegral telegramUserUserId))
        langs
        [ihamlet|
          #{attention} _{MsgYourSlotCancelled} ^{link}
          ^{slotDesc}
        |]
  updateWorkingSchedule pool False weekStart gid `catchError` (liftIO . hPrint stderr)
  admins <- runInPool pool getAdmins
  unless dayAvailable $
    forM_ admins $ \(TelegramUser auid lang _ _) -> ignoreError $ do
      let adminLangs = maybeToList lang
      slotFullDesc <- runInPool pool $ getSlotFullDesc slot
      void $
        send
          (ChatId (fromIntegral auid))
          adminLangs
          [ihamlet|
            #{attention} _{MsgSlotCancelled} ^{link}
            ^{slotFullDesc}
          |]

askCancelSlot ::
  [Lang] ->
  ConnectionPool ->
  ChatChannel ->
  MessageId ->
  ScheduledSlotId ->
  ClientM Bool
askCancelSlot langs pool ChatChannel {..} originalMsgId slotId = do
  Just slot <- runInPool pool $ get slotId
  Just volunteer <- runInPool pool $ get (scheduledSlotUser slot)
  Just TelegramUser {telegramUserUserId} <- runInPool pool $ get (volunteerUser volunteer)
  if channelChatId == ChatId (fromIntegral telegramUserUserId)
    then do
      slotDesc <- runInPool pool $ getSlotDesc slot
      msg <-
        responseResult
          <$> sendWithButtons
            channelChatId
            langs
            [ihamlet|
                #{attention} _{MsgSureCancel}
                ^{slotDesc}
              |]
            [[(__ MsgYesCancel, "cancel"), (__ MsgNoIWillCome, "will_come")]]

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
        flip catchError (liftIO . hPrint stderr) $ void $ deleteMessage channelChatId originalMsgId
        cancelSlot pool slotId
      pure doCancel
    else pure False

confirmSlot ::
  [Lang] ->
  ConnectionPool ->
  ChatChannel ->
  MessageId ->
  ScheduledSlotId ->
  ClientM ()
confirmSlot langs pool ChatChannel {..} originalMsgId slotId = do
  Just slot <- runInPool pool $ get slotId
  slotDesc <- runInPool pool $ getSlotDesc slot
  void $ deleteMessage channelChatId originalMsgId
  runInPool pool $ update slotId [ScheduledSlotState =. ScheduledSlotConfirmed]
  updateWorkingScheduleForDay pool False (scheduledSlotDay slot)
  void $
    sendWithButtons
      channelChatId
      langs
      [ihamlet|
        #{allGood} _{MsgConfirmedShort}
        ^{slotDesc}
      |]
      [[cancelSlotButton slotId]]

askForPermission :: ConnectionPool -> User -> ClientM ()
askForPermission pool User {userId = UserId uid} = do
  admins <- runInPool pool getAdmins
  Just (Entity tuid user) <-
    runInPool pool (getBy $ UniqueTelegramUser (fromIntegral uid))
  forM_ admins $ \(TelegramUser auid lang _ _) -> ignoreError $ do
    let langs = maybeToList lang
    void $
      insertCallbackQueryMessage pool
        =<< sendWithButtons
          (ChatId (fromIntegral auid))
          langs
          [ihamlet|
                #{hello} _{MsgVolunteerRequest $ renderUser user}
              |]
          [ [ ([ihamlet|#{allGood} _{MsgAllow}|], "admin_allow_" <> showSqlKey tuid),
              ([ihamlet|#{bad} _{MsgDecline}|], "admin_decline_" <> showSqlKey tuid)
            ]
          ]

allowVolunteer :: ConnectionPool -> TelegramUserId -> ClientM ()
allowVolunteer pool tuid = do
  (admins, Just user@TelegramUser {telegramUserLang, telegramUserUserId}) <-
    runInPool pool $ do
      void $ upsert Volunteer {volunteerUser = tuid} []
      admins <- getAdmins
      user <- get tuid
      pure (admins, user)
  forM_ admins $ \(TelegramUser auid lang _ _) -> ignoreError $ do
    let langs = maybeToList lang
    void $
      insertCallbackQueryMessage pool
        =<< sendWithButtons
          (ChatId (fromIntegral auid))
          langs
          [ihamlet|
                #{party} _{MsgNewVolunteer $ renderUser user}
              |]
          [ [ ([ihamlet|#{forbidden} _{MsgBan}|], "ban_" <> showSqlKey tuid)
            ]
          ]
  deleteCallbackQueryMessages pool ("allow_" <> showSqlKey tuid)
  void $
    send
      (ChatId (fromIntegral telegramUserUserId))
      (maybeToList telegramUserLang)
      [ihamlet|#{party} _{MsgVolunteer}|]

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
  forM_ admins $ \(TelegramUser auid lang _ _) -> ignoreError $ do
    let langs = maybeToList lang
    void $
      send
        (ChatId (fromIntegral auid))
        langs
        [ihamlet|#{allGood} _{MsgVolunteerRemoved (renderUser user)}|]

checkbox :: Bool -> Text
checkbox False = "☐"
checkbox True = "☑"

inlinePollMarkup ::
  Foldable t => Bool -> MessageButtons -> t Text -> MessageButtons
inlinePollMarkup doneable grid current =
  fmap (fmap (\(t, d) -> let visible = d `elem` current in ([ihamlet|#{checkbox visible} ^{t}|], d))) grid
    ++ [[(__ MsgDone, "done")] | doneable]
    ++ [[(__ MsgCancel, "cancel")]]

inlinePoll ::
  ChatChannel ->
  [Lang] ->
  ([Text] -> Bool) ->
  IHamlet ->
  MessageButtons ->
  [Text] ->
  ClientM (Maybe [Text])
inlinePoll ChatChannel {..} langs acceptable question grid initial = do
  Response {responseResult = msg} <-
    sendWithButtons channelChatId langs question $ inlinePollMarkup (acceptable initial) grid initial
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
                  ( editButtons
                      channelChatId
                      langs
                      (messageMessageId msg)
                      (inlinePollMarkup (acceptable lst') grid lst')
                  )
                  `catchError` const (pure ())
                _ <- a
                getPollResult lst'
          _ -> a >> getPollResult lst
  getPollResult initial <* deleteMessage channelChatId (messageMessageId msg)

sendOpenDaySchedule :: ConnectionPool -> Day -> GarageId -> [Entity OpenDay] -> ClientM ()
sendOpenDaySchedule pool weekStart garage days = do
  subs <- runInPool pool (selectList [] [] >>= mapM (get . subscriptionUser . entityVal))
  Just g@Garage {..} <- runInPool pool $ get garage
  updateWorkingSchedule pool False weekStart garage `catchError` (liftIO . hPrint stderr)
  asyncClientM_ $ forM_ subs $ \(Just (TelegramUser uid lang _ _)) -> ignoreError $ do
    let langs = maybeToList lang
    catchError
      ( do
          Response {responseResult = Message {messageMessageId = MessageId mid}} <-
            sendWithButtons
              (ChatId (fromIntegral uid))
              langs
              [ihamlet|#{news} _{MsgNewScheduleFor (renderGarage g)}|]
              [ [([ihamlet|_{showDate openDayDate}|], "signup_" <> showSqlKey did)]
                | Entity did OpenDay {..} <- days
              ]
          void $ runInPool pool $ insert $ CallbackQueryMultiChat uid (fromIntegral mid) ("schedule_" <> showSqlKey garage <> "_" <> pack (showGregorian weekStart))
          liftIO $ threadDelay 500000
      )
      (liftIO . hPrint stderr)

setOpenDays :: [Lang] -> ConnectionPool -> ChatChannel -> Maybe Day -> GarageId -> ClientM ()
setOpenDays langs pool chat day' gid = do
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
    langs
    (const True)
    [ihamlet|_{MsgGarageOpenDays (renderGarage g)}|]
    [[([ihamlet|_{showDate d}|], pack $ showGregorian d)] | d <- nextWeek]
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

lockSchedule :: [Lang] -> ConnectionPool -> ChatChannel -> Maybe Day -> GarageId -> ClientM ()
lockSchedule _langs pool ChatChannel {..} day' gid = do
  day <- liftIO $ case day' of
    Just d -> pure d
    Nothing -> nextWeekStart . localDay . zonedTimeToLocalTime <$> getZonedTime
  Just g@Garage {..} <- runInPool pool $ get gid
  let nextWeek = take 7 [day ..]
  let selector = [OpenDayGarage ==. gid, OpenDayDate <-. nextWeek]
  runInPool pool $ do
    updateWhere selector [OpenDayAvailable =. False]
  workingSchedule <- getWorkingSchedule pool day gid
  subscriptions <- runInPool pool (selectList [] [] >>= mapM (get . adminUser . entityVal))
  updateWorkingSchedule pool False day gid `catchError` (liftIO . hPrint stderr)
  asyncClientM_ $ forM_ subscriptions $ \(Just (TelegramUser suid _ _ _)) -> forM_ ["en", "ru"] $ \lang -> ignoreError $ do
    void $
      send
        (ChatId (fromIntegral suid))
        [lang]
        (renderChatSchedule g workingSchedule)
    liftIO $ threadDelay 500000

unlockSchedule :: [Lang] -> ConnectionPool -> ChatChannel -> Maybe Day -> GarageId -> ClientM ()
unlockSchedule _langs pool ChatChannel {..} day' gid = do
  day <- liftIO $ case day' of
    Just d -> pure d
    Nothing -> nextWeekStart . localDay . zonedTimeToLocalTime <$> getZonedTime
  Just Garage {..} <- runInPool pool $ get gid
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
    ("workingschedulethisweek", MsgCommandWorkingScheduleThisWeek),
    ("report", MsgCommandReport)
  ]

setCommands :: [(Text, BotMessage)] -> ChatChannel -> ClientM ()
setCommands commands ChatChannel {..} =
  forM_ knownLangs $ \lang -> do
    setMyCommands
      ( SetMyCommandsRequest
          { setMyCommandsCommands =
              fmap (\(c, d) -> BotCommand c (d |-> [lang])) commands,
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
                Just "/start" -> send channelChatId [] [ihamlet|Welcome, admin!|] $> True
                Just "/setopendays" -> (runInPool pool (selectList [] []) >>= mapM_ (setOpenDays langs pool chat Nothing . entityKey)) $> True
                Just "/lock" -> (runInPool pool (selectList [] []) >>= mapM_ (lockSchedule langs pool chat Nothing . entityKey)) $> True
                Just "/workingschedule" -> do
                  today <- localDay . zonedTimeToLocalTime <$> liftIO getZonedTime
                  runInPool pool (selectList [] []) >>= mapM_ (updateWorkingSchedule pool True (nextWeekStart today) . entityKey)
                  pure True
                Just "/workingschedulethisweek" -> do
                  today <- localDay . zonedTimeToLocalTime <$> liftIO getZonedTime
                  runInPool pool (selectList [] []) >>= mapM_ (updateWorkingSchedule pool True (thisWeekStart today) . entityKey)
                  pure True
                Just "/report" -> do
                  slots <- flattenSlots pool
                  file <- liftIO $ emptySystemTempFile "report.csv"
                  liftIO $ BS.writeFile file $ encode slots
                  _ <- sendDocument (toSendDocument (SomeChatId channelChatId) $ DocumentFile file "application/csv")
                  liftIO $ removeFile file
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
                  ["admin", "setopendays", g, d] -> setOpenDays langs pool chat (parseGregorian d) (readSqlKey g) $> True
                  ["admin", "lock", g, d] -> lockSchedule langs pool chat (parseGregorian d) (readSqlKey g) $> True
                  ["admin", "unlock", g, d] -> unlockSchedule langs pool chat (parseGregorian d) (readSqlKey g) $> True
                  _ -> pure False
            _ -> pure False

      let volunteerHandler volunteer = \case
            SomeNewMessage m@Message {..} ->
              case messageText of
                Just "/start" -> reply m langs (__ MsgVolunteer) $> True
                ((>>= stripPrefix "/start" >=> pure . T.splitOn "_") -> Just [d, s, e]) -> do
                  _ <- askCreateStep
                    langs
                    pool
                    chat
                    Nothing
                    volunteer
                    (readSqlKey d)
                    (fromJust $ parseHourMinutesM $ T.replace "-" ":" s)
                    (fromJust $ parseHourMinutesM $ T.replace "-" ":" e)
                  _ <- deleteMessage channelChatId messageMessageId
                  pure True
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
                          void $ reply m langs (__ MsgAck)
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
                    >> void (send channelChatId langs $ __ MsgGreet)
                      $> True
                _ -> pure False
            _ -> pure False

      let failHandler = \case SomeNewMessage m -> reply m langs (__ MsgNotRecognized) $> True; _ -> pure True

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
