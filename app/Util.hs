{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Move guards forward" #-}

module Util where

import Control.Concurrent (Chan)
import Control.Concurrent.Async
import Control.Monad
import Control.Monad.Except
import Control.Monad.IO.Class
import Control.Monad.Reader (ReaderT)
import Control.Monad.Reader.Class (ask)
import qualified Data.ByteString
import Data.ByteString.Lazy (ByteString)
import Data.Default (def)
import Data.Either (fromRight)
import Data.Functor ((<&>))
import qualified Data.List
import Data.Map (fromList)
import Data.Maybe (catMaybes, fromJust, fromMaybe, maybeToList)
import Data.Text (Text, pack, splitOn, unpack)
import Data.Text.Encoding (encodeUtf8)
import Data.Text.Lazy (fromStrict, toStrict)
import Data.Time
  ( Day (ModifiedJulianDay),
    LocalTime (localDay),
    TimeOfDay,
    TimeZone,
    UTCTime,
    ZonedTime (zonedTimeToLocalTime),
    dayOfWeek,
    getZonedTime,
    showGregorian,
    toGregorian, getCurrentTime, getCurrentTimeZone,
  )
import Data.Time.Calendar (DayOfWeek (Monday))
import Data.Time.Clock (diffTimeToPicoseconds)
import Data.Time.Format.ISO8601
  ( FormatExtension (ExtendedFormat),
    calendarFormat,
    formatParseM,
    formatShow,
    hourMinuteFormat,
  )
import Data.Time.LocalTime (LocalTime (LocalTime), localTimeToUTC, sinceMidnight)
import qualified Data.Time.Parsers as DTP
import Data.UUID (fromString, toText)
import Data.UUID.V5 (generateNamed)
import Database.Persist
import Database.Persist.Sql
import GHC.Conc (threadDelay)
import GHC.Float (double2Float)
import GHC.IO.Handle.FD (stderr)
import I18N
import Network.URI (URI (URI), URIAuth (URIAuth), parseURI)
import Persist
import Servant.Client (ClientM, runClientM)
import Symbols
import System.IO (hPrint)
import Telegram.Bot.API
import Telegram.Bot.Monadic
import Text.Hamlet
import Text.ICalendar.Printer (printICalendar)
import Text.ICalendar.Types (DTEnd (DTEndDateTime), DTStamp (DTStamp), DTStart (DTStartDateTime), DateTime (UTCDateTime), Description (Description), EventStatus (ConfirmedEvent, TentativeEvent), Geo (Geo), Location (Location), Organizer (Organizer), Priority (Priority), Summary (Summary), TimeTransparency (Transparent), UID (UID), VCalendar (vcEvents), VEvent (..))
import Text.Parsec (parse)
import Text.Shakespeare.I18N (Lang)
import Text.Shakespeare.Text (lt)
import qualified Data.ByteString.Lazy as BS
import qualified Data.Text as T

newtype BotConfig = BotConfig { icsDirectory :: Maybe FilePath }

instance MonadFail ClientM where
  fail e = liftIO (hPrint stderr e) >> liftIO (fail e)

ignoreError :: (MonadIO m, MonadError a m, Show a) => m () -> m ()
ignoreError = flip catchError (liftIO . hPrint stderr)

(+-+) :: (ToIHamlet t) => Text -> t -> IHamlet
symbol +-+ stuff = [ihamlet|#{symbol} ^{__ stuff}|]

ik :: [[InlineKeyboardButton]] -> SomeReplyMarkup
ik = SomeInlineKeyboardMarkup . InlineKeyboardMarkup

ikb :: Text -> Text -> InlineKeyboardButton
ikb t d =
  (labeledInlineKeyboardButton t) {inlineKeyboardButtonCallbackData = Just d}

getNewMessage :: ChatChannel -> ClientM (Either SomeUpdate Message)
getNewMessage ChatChannel {..} =
  getUpdate channelUpdateChannel <&> \case
    SomeNewMessage m -> Right m
    upd -> Left upd

send :: ChatId -> [Lang] -> IHamlet -> ClientM (Response Message)
send cid langs msg =
  sendMessage
    (defSendMessage (SomeChatId cid) (defaultRender langs msg))
      { sendMessageParseMode = Just HTML
      }

type MessageButtons = [[(IHamlet, Text)]]

sendWithButtons :: ChatId -> [Lang] -> IHamlet -> MessageButtons -> ClientM (Response Message)
sendWithButtons cid langs msg grid =
  sendMessage
    (defSendMessage (SomeChatId cid) (defaultRender langs msg))
      { sendMessageParseMode = Just HTML,
        sendMessageReplyMarkup = Just $ makeButtons langs grid
      }

edit :: ChatId -> [Lang] -> MessageId -> IHamlet -> ClientM (Response EditMessageResponse)
edit cid langs mid msg =
  editMessageText
    (defEditMessageText (defaultRender langs msg))
      { editMessageTextChatId = Just $ SomeChatId cid,
        editMessageTextMessageId = Just mid,
        editMessageTextParseMode = Just HTML
      }

editWithButtons :: ChatId -> [Lang] -> MessageId -> IHamlet -> MessageButtons -> ClientM (Response EditMessageResponse)
editWithButtons cid langs mid msg b = do
  editMessageText
    (defEditMessageText (defaultRender langs msg))
      { editMessageTextChatId = Just $ SomeChatId cid,
        editMessageTextMessageId = Just mid,
        editMessageTextReplyMarkup = Just $ makeButtons langs b,
        editMessageTextParseMode = Just HTML
      }

editButtons :: ChatId -> [Lang] -> MessageId -> MessageButtons -> ClientM (Response EditMessageResponse)
editButtons cid langs mid b = do
  editMessageReplyMarkup
    defEditMessageReplyMarkup
      { editMessageReplyMarkupChatId = Just $ SomeChatId cid,
        editMessageReplyMarkupReplyMarkup = Just $ makeButtons langs b,
        editMessageReplyMarkupMessageId = Just mid
      }

reply :: Message -> [Lang] -> IHamlet -> ClientM (Response Message)
reply (Message {messageChat = Chat {chatId}, messageMessageId}) langs rpl =
  sendMessage
    (defSendMessage (SomeChatId chatId) (defaultRender langs rpl))
      { sendMessageParseMode = Just HTML,
        sendMessageReplyToMessageId = Just messageMessageId
      }

ignoreUntilRight :: (Monad m) => m (Either a b) -> m b
ignoreUntilRight = (`untilRight` (const $ pure ()))

iterateUntilRight :: (Monad m) => a -> (a -> m (Either a b)) -> m b
iterateUntilRight value action =
  action value >>= \case
    Left value' -> iterateUntilRight value' action
    Right res -> pure res

getCallbackQueryWithData :: Chan SomeUpdate -> ClientM CallbackQuery
getCallbackQueryWithData channelUpdateChannel =
  ignoreUntilRight
    ( getUpdate channelUpdateChannel >>= \case
        SomeNewCallbackQuery
          q@CallbackQuery
            { callbackQueryId,
              callbackQueryData = Just _
            } ->
            answerCallbackQuery (defAnswerCallbackQuery callbackQueryId)
              >> pure (Right q)
        _ -> pure $ Left ()
    )

showHourMinutes :: TimeOfDay -> Text
showHourMinutes = pack . formatShow (hourMinuteFormat ExtendedFormat)

parseHourMinutesM :: (MonadFail m) => Text -> m TimeOfDay
parseHourMinutesM = formatParseM (hourMinuteFormat ExtendedFormat) . unpack

parseGregorian :: (MonadFail m) => Text -> m Day
parseGregorian = formatParseM (calendarFormat ExtendedFormat) . unpack

thisWeekStart :: Day -> Day
thisWeekStart day = head $ dropWhile ((/= Monday) . dayOfWeek) [day, pred day ..]

nextWeekStart :: Day -> Day
nextWeekStart day = head $ dropWhile ((/= Monday) . dayOfWeek) [succ day ..]

chunksOf :: Int -> [a] -> [[a]]
chunksOf n lst =
  case Prelude.splitAt n lst of
    (l, []) -> [l]
    (l, l') -> l : chunksOf n l'

renderUser :: TelegramUser -> Html
renderUser TelegramUser {telegramUserUsername = Just username} = [shamlet|@#{username}|]
renderUser TelegramUser {telegramUserUserId = uid, telegramUserFullName} = [shamlet|<a href="tg://user?id=#{uid}">#{telegramUserFullName}|]

renderGarage :: Garage -> Html
renderGarage Garage {..} = [shamlet|#{garageName} (<a href="#{garageLink}">#{garageAddress}</a>)|]

renderGarageName :: Garage -> Html
renderGarageName Garage {..} = [shamlet|#{garageName}|]

renderGarageText :: Garage -> Text
renderGarageText Garage {..} = toStrict [lt|#{garageName} (#{garageAddress})|]

getSlotDesc ::
  (MonadIO m, MonadFail m) =>
  ScheduledSlot ->
  ReaderT SqlBackend m IHamlet
getSlotDesc ScheduledSlot {..} = do
  Just OpenDay {..} <- get scheduledSlotDay
  Just Garage {..} <- get openDayGarage
  pure
    [ihamlet|
    #{house}<u>_{MsgWhere}:</u> #{garageAddress}
    #{clock}<u>_{MsgWhen}:</u> _{showDate openDayDate}, #{showHourMinutes scheduledSlotStartTime}—#{showHourMinutes scheduledSlotEndTime}
  |]

getSlotFullDesc ::
  (MonadIO m, MonadFail m) =>
  ScheduledSlot ->
  ReaderT SqlBackend m IHamlet
getSlotFullDesc slot@ScheduledSlot {..} = do
  Just Volunteer {..} <- get scheduledSlotUser
  Just user <- get volunteerUser
  slotDesc <- getSlotDesc slot
  pure
    [ihamlet|
    #{person} <u>_{MsgWho}:</u> #{renderUser user}
    ^{slotDesc}
  |]

getAdmins :: ReaderT SqlBackend IO [TelegramUser]
getAdmins = do
  admins <- selectList [] []
  catMaybes <$> mapM get (fmap (adminUser . entityVal) admins)

renderState :: ScheduledSlotState -> Text
renderState ScheduledSlotCreated = news
renderState (ScheduledSlotAwaitingConfirmation _) = hourglass
renderState ScheduledSlotUnconfirmed = attention
renderState ScheduledSlotConfirmed = allGood
renderState ScheduledSlotFinished {} = finished
renderState ScheduledSlotChecklistComplete {visitors} = finished <> " (" <> pack (show visitors) <> " " <> people <> ")"

type WorkingScheduleList = [(Day, [(TelegramUser, TimeOfDay, TimeOfDay, ScheduledSlotState)])]

type ScheduleList = [(Day, [(TimeOfDay, TimeOfDay)])]

renderWorkingSchedule :: Garage -> Bool -> Bool -> WorkingScheduleList -> ScheduleList -> IHamlet
renderWorkingSchedule garage isLocked isAdmin days schedule = $(ihamletFile "templates/working_schedule.ihamlet")

getWeek :: ConnectionPool -> Day -> GarageId -> ClientM [Entity OpenDay]
getWeek pool weekStart garage = do
  let selector = [OpenDayGarage ==. garage, OpenDayDate <-. take 7 [weekStart ..]]
  runInPool pool $
    selectList selector []

-- | Merge intersecting intervals together
-- | This assumes that all intervals are valid, i.e. (start, end) | start <= end
mergeIntervals :: (Ord a) => [(a, a)] -> [(a, a)]
mergeIntervals slots = go (Data.List.sort slots)
  where
    go [] = []
    go [a] = [a] -- Single interval
    go ((sa, ea) : (sb, eb) : xs)
      | eb <= ea = go ((sa, ea) : xs) -- Second is contain in first, merge and continue
      | sb <= ea = go ((sa, eb) : xs) -- Intersecting, merge and continue
      | otherwise = (sa, ea) : go ((sb, eb) : xs) -- Non-intersecting, keep the first interval as-is and continue

renderSchedule :: Garage -> [(Day, [(TimeOfDay, TimeOfDay)])] -> IHamlet
renderSchedule g s = $(ihamletFile "templates/schedule.ihamlet")

renderChatSchedule :: Garage -> WorkingScheduleList -> IHamlet
renderChatSchedule garage days = $(ihamletFile "templates/chat_schedule.ihamlet")

getSchedule :: ConnectionPool -> Day -> GarageId -> ClientM ScheduleList
getSchedule pool weekStart garage = do
  getWeek pool weekStart garage
    >>= mapM
      ( \(Entity openDay OpenDay {..}) -> do
          slots <- runInPool pool $ selectList [ScheduledSlotDay ==. openDay] []
          pure
            ( openDayDate,
              [ (scheduledSlotStartTime, scheduledSlotEndTime)
                | Entity _ ScheduledSlot {..} <- slots
              ]
            )
      )

getWorkingSchedule :: ConnectionPool -> Day -> GarageId -> ClientM WorkingScheduleList
getWorkingSchedule pool weekStart garage = do
  getWeek pool weekStart garage
    >>= runInPool pool
      . mapM
        ( \(Entity day OpenDay {..}) -> do
            slots <- runInPool pool $ selectList [ScheduledSlotDay ==. day] []
            times <- forM slots $ \(Entity _ ScheduledSlot {..}) -> do
              Just Volunteer {..} <- get scheduledSlotUser
              Just u <- get volunteerUser
              pure (u, scheduledSlotStartTime, scheduledSlotEndTime, scheduledSlotState)
            pure (openDayDate, times)
        )

checkLocked :: ConnectionPool -> Day -> GarageId -> ClientM Bool
checkLocked pool weekStart garage = not . all (\(Entity _ OpenDay {openDayAvailable}) -> openDayAvailable) <$> getWeek pool weekStart garage

updateWorkingSchedule :: ConnectionPool -> BotConfig -> Bool -> Day -> GarageId -> ClientM ()
updateWorkingSchedule pool BotConfig {..} recreate weekStart garage = do
  admins <- runInPool pool (selectList [] [] >>= mapM (get . adminUser . entityVal))
  Just g <- runInPool pool $ get garage
  isLocked <- checkLocked pool weekStart garage
  let s = "working_schedule_" <> showSqlKey garage <> "_" <> pack (showGregorian weekStart)
  forM_ (catMaybes admins) $ \(TelegramUser auid lang _ _) -> do
    liftIO $ threadDelay 100000 -- Telegram rate limits
    let langs = maybeToList lang
    scheduleIHamlet <- renderWorkingSchedule g isLocked True <$> getWorkingSchedule pool weekStart garage <*> getSchedule pool weekStart garage
    let t = defaultRender langs scheduleIHamlet
    messages <- runInPool pool $ selectList [CallbackQueryMultiChatCallbackQuery ==. s, CallbackQueryMultiChatChatId ==. auid] []
    let lockButton =
          if not isLocked
            then ([ihamlet|#{locked} _{MsgLock}|], "admin_lock_" <> showSqlKey garage <> "_" <> pack (showGregorian weekStart))
            else ([ihamlet|#{unlocked} _{MsgUnlock}|], "admin_unlock_" <> showSqlKey garage <> "_" <> pack (showGregorian weekStart))
    let changeButton = ([ihamlet|#{change} _{MsgEdit}|], "admin_setopendays_" <> showSqlKey garage <> "_" <> pack (showGregorian weekStart))
    let buttons = makeButtons langs [[changeButton], [lockButton]]

    flip catchError (liftIO . hPrint stderr) $ case (recreate, messages) of
      (False, [Entity _ CallbackQueryMultiChat {..}]) -> do
        flip catchError (liftIO . hPrint stderr) $
          void $
            editMessageText
              (defEditMessageText t)
                { editMessageTextMessageId = Just (MessageId $ fromIntegral callbackQueryMultiChatMsgId),
                  editMessageTextChatId = Just $ SomeChatId (ChatId $ fromIntegral auid),
                  editMessageTextParseMode = Just HTML,
                  editMessageTextReplyMarkup = Just buttons
                }
      (_, msgs) -> do
        MessageId mid <-
          messageMessageId . responseResult
            <$> sendMessage
              (defSendMessage (SomeChatId (ChatId (fromIntegral auid))) t)
                { sendMessageParseMode = Just HTML,
                  sendMessageReplyMarkup = Just buttons
                }
        void $ runInPool pool $ insert $ CallbackQueryMultiChat auid (fromIntegral mid) s
        forM_ msgs $ \(Entity _ CallbackQueryMultiChat {..}) -> do
          flip catchError (liftIO . hPrint stderr) $ void $ deleteMessage (ChatId (fromIntegral auid)) (MessageId $ fromIntegral callbackQueryMultiChatMsgId)
          runInPool pool $ deleteWhere [CallbackQueryMultiChatChatId ==. auid, CallbackQueryMultiChatMsgId ==. callbackQueryMultiChatMsgId]
  case icsDirectory of
    Nothing -> pure ()
    Just dir -> do
      now <- liftIO getCurrentTime
      zone <- liftIO getCurrentTimeZone
      today <- localDay . zonedTimeToLocalTime <$> liftIO getZonedTime
      schedule <-
        runInPool pool $ do
          selectList [OpenDayGarage ==. garage, OpenDayDate >=. today] []
            >>= mapM
              ( \(Entity openDay OpenDay {..}) -> do
                  slots <- runInPool pool $ selectList [ScheduledSlotDay ==. openDay] []
                  pure
                    ( openDayDate,
                      [ (scheduledSlotStartTime, scheduledSlotEndTime)
                        | Entity _ ScheduledSlot {..} <- slots
                      ]
                    )
              )
      let ical = iCalendar now zone g schedule
      liftIO $ BS.writeFile (dir ++ "/" ++ T.unpack (garageName g) ++ ".ics") $ printICalendar def ical

updateWorkingScheduleForDay :: ConnectionPool -> BotConfig -> Bool -> OpenDayId -> ClientM ()
updateWorkingScheduleForDay pool botConfig recreate dayId = do
  (weekStart, gid) <-
    runInPool pool $ do
      Just OpenDay {..} <- get dayId
      let weekStart = thisWeekStart openDayDate
      pure (weekStart, openDayGarage)
  updateWorkingSchedule pool botConfig recreate weekStart gid

makeButtons :: [Lang] -> [[(IHamlet, Text)]] -> SomeReplyMarkup
makeButtons langs grid = ik (fmap (\(text, callback) -> ikb (defaultRender langs text) callback) <$> grid)

parseWeirdDate :: Text -> Maybe Day
parseWeirdDate s = case splitOn "-" s of
  [y] -> parseGregorian (y <> "-01-01")
  [y, m] -> parseGregorian (y <> "-" <> m <> "-01")
  [y, m, d] -> parseGregorian s
  _ -> Nothing

-- | Flatten all slots which are completed
flattenSlots ::
  (MonadIO m) =>
  ConnectionPool ->
  Maybe Day ->
  Maybe Day ->
  m
    [ ( Text,
        -- \| Name of the garage
        Text,
        -- \| Date of the slot
        Text,
        -- \| Starting time
        Text,
        -- \| End time
        Text,
        -- \| Volunteer's name
        Text,
        -- \| Volunteer's handle
        Text,
        -- \| Slot state
        Text
      )
    ]
-- \| The amount of visitors (if available)

flattenSlots pool start end = runInPool pool $ do
  slots <- selectList [] []
  let start' = fromMaybe (ModifiedJulianDay 0) start
  end' <- flip fromMaybe end . succ . localDay . zonedTimeToLocalTime <$> liftIO getZonedTime
  maybeSlots <- forM slots $ \(Entity _ ScheduledSlot {..}) ->
    get scheduledSlotDay >>= \case
      Just (OpenDay {openDayGarage, openDayDate})
        | openDayDate >= start' && openDayDate < end' -> do
            Just (Garage {garageName}) <- get openDayGarage
            (fullName, userName) <-
              get scheduledSlotUser >>= \case
                Nothing -> pure (dm, dm)
                Just (Volunteer {volunteerUser}) ->
                  get volunteerUser >>= \case
                    Nothing -> pure (dm, dm)
                    Just (TelegramUser {telegramUserFullName, telegramUserUsername}) -> pure (telegramUserFullName, fromMaybe "" telegramUserUsername)
            let visitors = case scheduledSlotState of
                  (ScheduledSlotChecklistComplete v) -> Just v
                  _ -> Nothing
            pure $ Just (garageName, pack $ showGregorian openDayDate, showHourMinutes scheduledSlotStartTime, showHourMinutes scheduledSlotEndTime, fullName, userName, renderStateText scheduledSlotState, pack $ maybe "" show visitors)
      _ -> pure Nothing
  pure $ catMaybes maybeSlots
  where
    dm = "<deleted>"

renderStateText :: ScheduledSlotState -> Text
renderStateText ScheduledSlotCreated = "Created"
renderStateText ScheduledSlotAwaitingConfirmation {} = "Awaiting Confirmation"
renderStateText ScheduledSlotUnconfirmed = "Unconfirmed"
renderStateText ScheduledSlotConfirmed = "Confirmed"
renderStateText ScheduledSlotFinished {} = "Finished"
renderStateText ScheduledSlotChecklistComplete {} = "Completed"

namespaceParkiArMinda = fromJust $ fromString "c285d7f8-1dbe-4fbd-9115-4f0c7700664f"

-- | Get an ICalendar representation of a schedule
iCalendar :: UTCTime -> TimeZone -> Garage -> [(Day, [(TimeOfDay, TimeOfDay)])] -> VCalendar
iCalendar now zone (Garage {..}) slots =
  ( def
      { vcEvents =
          fromList
            [ ((fromStrict $ toText uuid, Nothing), event)
              | (day, subslots) <- slots,
                (start, end) <- mergeIntervals subslots,
                let (y, m, d) = toGregorian day,
                let diffSeconds d = fromIntegral (diffTimeToPicoseconds (sinceMidnight d) `div` 10 ^ 12),
                -- UUID is generated deterministically from garage name and event datetime
                let uuid = generateNamed guuid [fromIntegral y, fromIntegral m, fromIntegral d, diffSeconds start, diffSeconds end],
                let event =
                      VEvent
                        { veDTStamp = DTStamp now def,
                          veUID = UID (fromStrict $ toText uuid) def,
                          veClass = def,
                          veDTStart = Just (DTStartDateTime (UTCDateTime (localTimeToUTC zone $ LocalTime day start)) def),
                          veCreated = def,
                          veGeo = def,
                          veDescription =
                            Just $
                              Description ("The " <> fromStrict garageName <> " garage is open") Nothing Nothing def,
                          veLastMod = Nothing,
                          veLocation =
                            Just $
                              Text.ICalendar.Types.Location
                                (fromStrict (garageName <> " parki ar minda garage\n" <> garageAddress<>"\n"<>garageLink))
                                (parseURI (T.unpack garageLink))
                                Nothing
                                def,
                          veOrganizer =
                            Just $
                              Organizer
                                (fromJust $ parseURI "MAILTO:parkiarminda@gmail.com")
                                (Just "parkiarminda@gmail.com")
                                Nothing
                                Nothing
                                Nothing
                                def,
                          vePriority = Priority 9 def, -- Least priority
                          veSeq = def,
                          veStatus = def,
                          veSummary =
                            Just $
                              Summary ("parki ar minda — " <> fromStrict garageName) Nothing Nothing def,
                          veTransp = Transparent def,
                          veUrl = Nothing,
                          veRecurId = Nothing,
                          veRRule = def,
                          veDTEndDuration =
                            Just $
                              Left (DTEndDateTime (UTCDateTime (localTimeToUTC zone $ LocalTime day end)) def),
                          veAttach = def,
                          veAttendee = def,
                          veCategories = def,
                          veComment = def,
                          veContact = def,
                          veExDate = def,
                          veRStatus = def,
                          veRelated = def,
                          veResources = def,
                          veRDate = def,
                          veAlarms = def,
                          veOther = def
                        }
            ]
      }
  )
  where
    guuid = generateNamed namespaceParkiArMinda (Data.ByteString.unpack (encodeUtf8 garageName))

asyncClientM_ :: ClientM () -> ClientM ()
asyncClientM_ c = do
  clientEnv <- ask
  void $ liftIO $ async $ runClientM c clientEnv

parseTimeOfDay :: String -> TimeOfDay
parseTimeOfDay = fromRight undefined . parse DTP.timeOfDay "" . pack

parseDayOfWeek :: String -> DayOfWeek
parseDayOfWeek = read
