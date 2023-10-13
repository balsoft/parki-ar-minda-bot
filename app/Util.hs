{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Util where

import Control.Concurrent (Chan)
import Control.Monad.Except
import Control.Monad.Reader (ReaderT)
import Data.Functor ((<&>))
import qualified Data.List
import Data.Maybe (catMaybes, maybeToList, fromMaybe)
import Data.Text (Text, pack, unpack)
import Data.Text.Lazy (toStrict)
import Data.Time (Day, TimeOfDay, dayOfWeek, showGregorian)
import Data.Time.Calendar (DayOfWeek (Monday))
import Data.Time.Format.ISO8601
  ( FormatExtension (ExtendedFormat),
    calendarFormat,
    formatParseM,
    formatShow,
    hourMinuteFormat,
  )
import Database.Persist
import Database.Persist.Sql
import GHC.Conc (threadDelay)
import I18N
import Persist
import Servant.Client (ClientM, runClientM)
import Symbols
import Telegram.Bot.API
import Telegram.Bot.Monadic
import Text.Hamlet
import Text.Shakespeare.I18N (Lang)
import Text.Shakespeare.Text (lt)
import Control.Monad.Reader.Class (ask)
import Control.Concurrent.Async

instance MonadFail ClientM where
  fail e = liftIO (print e) >> liftIO (fail e)

ignoreError :: (MonadIO m, MonadError a m, Show a) => m () -> m ()
ignoreError = flip catchError (liftIO . print)

(+-+) :: ToIHamlet t => Text -> t -> IHamlet
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
    (sendMessageRequest cid (defaultRender langs msg))
      { sendMessageParseMode = Just HTML
      }

type MessageButtons = [[(IHamlet, Text)]]

sendWithButtons :: ChatId -> [Lang] -> IHamlet -> MessageButtons -> ClientM (Response Message)
sendWithButtons cid langs msg grid =
  sendMessage
    (sendMessageRequest cid (defaultRender langs msg))
      { sendMessageParseMode = Just HTML,
        sendMessageReplyMarkup = Just $ makeButtons langs grid
      }

edit :: ChatId -> [Lang] -> MessageId -> IHamlet -> ClientM (Response EditMessageResponse)
edit cid langs mid msg =
  editMessageText
    (editMessageTextRequest (defaultRender langs msg))
      { editMessageTextChatId = Just $ SomeChatId cid,
        editMessageTextMessageId = Just mid,
        editMessageTextParseMode = Just HTML
      }

editWithButtons :: ChatId -> [Lang] -> MessageId -> IHamlet -> MessageButtons -> ClientM (Response EditMessageResponse)
editWithButtons cid langs mid msg b = do
  editMessageText
    (editMessageTextRequest (defaultRender langs msg))
      { editMessageTextChatId = Just $ SomeChatId cid,
        editMessageTextMessageId = Just mid,
        editMessageTextReplyMarkup = Just $ makeButtons langs b,
        editMessageTextParseMode = Just HTML
      }

editButtons :: ChatId -> [Lang] -> MessageId -> MessageButtons -> ClientM (Response EditMessageResponse)
editButtons cid langs mid b = do
  editMessageReplyMarkup
    (editMessageReplyMarkupRequest $ Just $ makeButtons langs b)
      { editMessageReplyMarkupChatId = Just $ SomeChatId cid,
        editMessageReplyMarkupMessageId = Just mid
      }

reply :: Message -> [Lang] -> IHamlet -> ClientM (Response Message)
reply (Message {messageChat = Chat {chatId}, messageMessageId}) langs rpl =
  sendMessage
    (sendMessageRequest chatId (defaultRender langs rpl))
      { sendMessageParseMode = Just HTML,
        sendMessageReplyToMessageId = Just messageMessageId
      }

ignoreUntilRight :: Monad m => m (Either a b) -> m b
ignoreUntilRight = (`untilRight` (const $ pure ()))

iterateUntilRight :: Monad m => a -> (a -> m (Either a b)) -> m b
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
            answerCallbackQuery (answerCallbackQueryRequest callbackQueryId)
              >> pure (Right q)
        _ -> pure $ Left ()
    )

showHourMinutes :: TimeOfDay -> Text
showHourMinutes = pack . formatShow (hourMinuteFormat ExtendedFormat)

parseHourMinutesM :: MonadFail m => Text -> m TimeOfDay
parseHourMinutesM = formatParseM (hourMinuteFormat ExtendedFormat) . unpack

parseGregorian :: MonadFail m => Text -> m Day
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
mergeIntervals :: Ord a => [(a, a)] -> [(a, a)]
mergeIntervals slots = go (Data.List.sort slots)
  where
    go [] = []
    go [a] = [a]
    go ((sa, ea) : (sb, eb) : xs) = if sb <= ea then go ((sa, eb) : xs) else (sa, ea) : go ((sb, eb) : xs)

renderSchedule :: Garage -> [(Day, [(TimeOfDay, TimeOfDay)])] -> IHamlet
renderSchedule g s = $(ihamletFile "templates/schedule.ihamlet")

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

updateWorkingSchedule :: ConnectionPool -> Bool -> Day -> GarageId -> ClientM ()
updateWorkingSchedule pool recreate weekStart garage = do
  admins <- runInPool pool (selectList [] [] >>= mapM (get . adminUser . entityVal))
  Just g <- runInPool pool $ get garage
  isLocked <- checkLocked pool weekStart garage
  let s = "working_schedule_" <> showSqlKey garage <> "_" <> pack (showGregorian weekStart)
  forM_ admins $ \(Just (TelegramUser auid lang _ _)) -> do
    liftIO $ threadDelay 100000 -- Telegram rate limits
    let langs = maybeToList lang
    scheduleIHamlet <- renderWorkingSchedule g isLocked True <$> getWorkingSchedule pool weekStart garage <*> getSchedule pool weekStart garage
    let t = defaultRender langs scheduleIHamlet
    messages <- runInPool pool $ selectList [CallbackQueryMultiChatCallbackQuery ==. s, CallbackQueryMultiChatChatId ==. auid] []
    let buttons =
          makeButtons langs $
            if not isLocked
              then [[([ihamlet|#{locked} _{MsgLock}|], "admin_lock_" <> showSqlKey garage <> "_" <> pack (showGregorian weekStart))]]
              else [[([ihamlet|#{unlocked} _{MsgUnlock}|], "admin_unlock_" <> showSqlKey garage <> "_" <> pack (showGregorian weekStart))]]
    flip catchError (liftIO . print) $ case (recreate, messages) of
      (False, [Entity _ CallbackQueryMultiChat {..}]) -> do
        flip catchError (liftIO . print) $
          void $
            editMessageText
              (editMessageTextRequest t)
                { editMessageTextMessageId = Just (MessageId $ fromIntegral callbackQueryMultiChatMsgId),
                  editMessageTextChatId = Just $ SomeChatId (ChatId $ fromIntegral auid),
                  editMessageTextParseMode = Just HTML,
                  editMessageTextReplyMarkup = Just buttons
                }
      (_, msgs) -> do
        MessageId mid <-
          messageMessageId . responseResult
            <$> sendMessage
              (sendMessageRequest (ChatId (fromIntegral auid)) t)
                { sendMessageParseMode = Just HTML,
                  sendMessageReplyMarkup = Just buttons
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

makeButtons :: [Lang] -> [[(IHamlet, Text)]] -> SomeReplyMarkup
makeButtons langs grid = ik (fmap (\(text, callback) -> ikb (defaultRender langs text) callback) <$> grid)

-- | Flatten all slots which are completed
flattenSlots ::
  MonadIO m =>
  ConnectionPool ->
  m
    [ ( Text, -- | Name of the garage
        Text, -- | Date of the slot
        Text, -- | Starting time
        Text, -- | End time
        Text, -- | Volunteer's name
        Text, -- | Volunteer's handle
        Text, -- | Slot state
        Text  -- | The amount of visitors (if available)
      )
    ]
flattenSlots pool = runInPool pool $ do
  slots <- selectList [] []
  forM slots $ \(Entity _ ScheduledSlot {..}) -> do
    get scheduledSlotDay >>= \case
      Nothing -> pure (dm, dm, dm, dm, dm, dm, dm, dm)
      Just (OpenDay {openDayGarage, openDayDate}) -> do
        Just (Garage {garageName}) <- get openDayGarage
        (fullName, userName) <- get scheduledSlotUser >>= \case
            Nothing -> pure (dm, dm)
            Just (Volunteer {volunteerUser}) ->
              get volunteerUser >>= \case
                Nothing -> pure (dm, dm)
                Just (TelegramUser {telegramUserFullName, telegramUserUsername}) -> pure (telegramUserFullName, fromMaybe "" telegramUserUsername)
        let visitors = case scheduledSlotState of
              (ScheduledSlotChecklistComplete v) -> Just v
              _ -> Nothing
        pure (garageName, pack $ showGregorian openDayDate, showHourMinutes scheduledSlotStartTime, showHourMinutes scheduledSlotEndTime, fullName, userName, renderStateText scheduledSlotState, pack $ maybe "" show visitors)

  where dm = "<deleted>"
renderStateText :: ScheduledSlotState -> Text
renderStateText ScheduledSlotCreated = "Created"
renderStateText ScheduledSlotAwaitingConfirmation {} = "Awaiting Confirmation"
renderStateText ScheduledSlotUnconfirmed = "Unconfirmed"
renderStateText ScheduledSlotConfirmed = "Confirmed"
renderStateText ScheduledSlotFinished {} = "Finished"
renderStateText ScheduledSlotChecklistComplete {} = "Completed"

asyncClientM_ :: ClientM () -> ClientM ()
asyncClientM_ c = do
  clientEnv <- ask
  void $ liftIO $ async $ runClientM c clientEnv
