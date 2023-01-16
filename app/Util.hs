{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Util where

import Control.Concurrent (Chan)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader (ReaderT)
import Data.Functor ((<&>))
import Data.Maybe (catMaybes)
import Data.Text (Text, pack, unpack)
import Data.Text.Lazy (toStrict)
import Data.Time (Day, TimeOfDay, dayOfWeek)
import Data.Time.Calendar (DayOfWeek (Monday))
import Data.Time.Format.ISO8601
  ( FormatExtension (ExtendedFormat),
    calendarFormat,
    formatParseM,
    formatShow,
    hourMinuteFormat,
  )
import Database.Persist
import Database.Persist.Sqlite (SqlBackend)
import I18N
import Persist
import Servant.Client (ClientM)
import Symbols (clock, house, person)
import Telegram.Bot.API
import Telegram.Bot.Monadic
import Text.Blaze
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Text.Hamlet
import Text.Shakespeare.I18N (Lang)
import Text.Shakespeare.Text (lt)

instance MonadFail ClientM where
  fail e = liftIO (print e) >> liftIO (fail e)

type IHamlet = (BotMessage -> Html) -> (() -> ()) -> Html

defaultLayout :: [Lang] -> IHamlet -> Html
defaultLayout langs f = f (preEscapedText <$> tr langs) (const ())

defaultRender :: [Lang] -> IHamlet -> Text
defaultRender langs = toStrict . renderHtml . defaultLayout langs

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

send :: ChatId -> [Lang] -> BotMessage -> ClientM (Response Message)
send cid langs msg =
  sendMessage
    (sendMessageRequest cid (tr langs msg))
      { sendMessageParseMode = Just HTML
      }

reply :: Message -> [Lang] -> BotMessage -> ClientM (Response Message)
reply (Message {messageChat = Chat {chatId}, messageMessageId}) langs rpl =
  sendMessage
    (sendMessageRequest chatId (tr langs rpl))
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
nextWeekStart day = head $ dropWhile ((/= Monday) . dayOfWeek) [day ..]

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
  [Lang] ->
  ScheduledSlot ->
  ReaderT SqlBackend m Html
getSlotDesc langs ScheduledSlot {..} = do
  Just OpenDay {..} <- get scheduledSlotDay
  Just Garage {..} <- get openDayGarage
  pure $
    defaultLayout
      langs
      [ihamlet|
    #{house}<u>_{MsgWhere}:</u> #{garageAddress}
    #{clock}<u>_{MsgWhen}:</u> #{showDay langs openDayDate}, #{showHourMinutes scheduledSlotStartTime}—#{showHourMinutes scheduledSlotEndTime}
  |]

getSlotFullDesc ::
  (MonadIO m, MonadFail m) =>
  [Lang] ->
  ScheduledSlot ->
  ReaderT SqlBackend m Html
getSlotFullDesc langs slot@ScheduledSlot {..} = do
  Just Volunteer {..} <- get scheduledSlotUser
  Just user <- get volunteerUser
  slotDesc <- getSlotDesc langs slot
  pure $
    defaultLayout
      langs
      [ihamlet|
    #{person} <u>_{MsgWho}:</u> #{renderUser user}
    #{slotDesc}
  |]

getAdmins :: ReaderT SqlBackend IO [TelegramUser]
getAdmins = do
  admins <- selectList [] []
  catMaybes <$> mapM get (fmap (adminUser . entityVal) admins)
