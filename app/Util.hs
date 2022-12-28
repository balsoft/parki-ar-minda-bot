{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Util where

import Control.Concurrent (Chan)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader (ReaderT)
import Data.Functor ((<&>))
import Data.Text (Text, pack, unpack)
import Data.Time (TimeOfDay, Day)
import Data.Time.Format.ISO8601
  ( FormatExtension (ExtendedFormat),
    formatParseM,
    formatShow,
    hourMinuteFormat, calendarFormat,
  )
import Database.Persist
import Database.Persist.Sqlite (SqlBackend)
import I18N
import Persist
import Servant.Client (ClientM)
import Telegram.Bot.API
import Telegram.Bot.Monadic
import Text.Shakespeare.I18N (Lang)
import qualified Data.Text as T
import Data.Maybe (catMaybes)

instance MonadFail ClientM where
  fail e = liftIO (print e) >> liftIO (fail e)

ik :: [[InlineKeyboardButton]] -> SomeReplyMarkup
ik = SomeInlineKeyboardMarkup . InlineKeyboardMarkup

ikb :: Text -> Text -> InlineKeyboardButton
ikb text d =
  (labeledInlineKeyboardButton text) {inlineKeyboardButtonCallbackData = Just d}

getNewMessage :: ChatChannel -> ClientM (Either SomeUpdate Message)
getNewMessage ChatChannel {..} =
  getUpdate channelUpdateChannel <&> \case
    SomeNewMessage m -> Right m
    upd -> Left upd

send :: ChatId -> [Lang] -> BotMessage -> ClientM (Response Message)
send cid langs msg =
  sendMessage
    (sendMessageRequest cid (tr langs msg))
      { sendMessageParseMode = Just MarkdownV2
      }

reply :: Message -> [Lang] -> BotMessage -> ClientM (Response Message)
reply (Message {messageChat = Chat {chatId}, messageMessageId}) langs rpl =
  sendMessage
    (sendMessageRequest chatId (tr langs rpl))
      { sendMessageParseMode = Just MarkdownV2,
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

checkIf :: Bool -> Text
checkIf b =
  if b
    then "✔ "
    else ""

attention :: Text
attention = "⚠ "

allGood :: Text
allGood = "✅ "

forbidden :: Text
forbidden = "🚫 "

bad :: Text
bad = "❌ "

news :: Text
news = "⚡ "

chunksOf :: Int -> [a] -> [[a]]
chunksOf n lst =
  case Prelude.splitAt n lst of
    (l, []) -> [l]
    (l, l') -> l : chunksOf n l'

renderUser :: TelegramUser -> Text
renderUser TelegramUser {telegramUserUsername = Just username} = "@" <> username
renderUser TelegramUser {telegramUserUserId = uid, telegramUserFullName} =
  "["
    <> T.replace "]" "\\]" (T.replace "\\" "\\\\" telegramUserFullName)
    <> "](tg://user?id="
    <> pack (show uid)
    <> ")"

getSlotDesc ::
  (MonadIO m, MonadFail m) =>
  [Lang] ->
  ScheduledSlot ->
  ReaderT SqlBackend m Text
getSlotDesc langs ScheduledSlot {..} = do
  Just OpenDay {..} <- get scheduledSlotDay
  Just Garage {..} <- get openDayGarage
  pure $
    tr
      langs
      ( MsgSlotDescription
          garageAddress
          ( showDay langs openDayDate
              <> ", "
              <> showHourMinutes scheduledSlotStartTime
              <> "—"
              <> showHourMinutes scheduledSlotEndTime
          )
      )

getSlotFullDesc ::
  (MonadIO m, MonadFail m) =>
  [Lang] ->
  ScheduledSlot ->
  ReaderT SqlBackend m Text
getSlotFullDesc langs ScheduledSlot {..} = do
  Just Volunteer {..} <- get scheduledSlotUser
  Just user <- get volunteerUser
  Just OpenDay {..} <- get scheduledSlotDay
  Just Garage {..} <- get openDayGarage
  pure $
    tr
      langs
      ( MsgSlotFullDescription
          (renderUser user)
          garageAddress
          ( showDay langs openDayDate
              <> ", "
              <> showHourMinutes scheduledSlotStartTime
              <> "—"
              <> showHourMinutes scheduledSlotEndTime
          )
      )


getAdmins :: ReaderT SqlBackend IO [TelegramUser]
getAdmins = do
  admins <- selectList [] []
  catMaybes <$> mapM get (fmap (adminUser . entityVal) admins)
