{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module ReminderBot
  ( reminderBot,
  )
where

import Control.Monad (forM_, void, when)
import Control.Monad.IO.Unlift (MonadIO (liftIO))
import Data.Maybe (maybeToList)
import Data.Time
import Database.Persist
import Database.Persist.Sqlite (ConnectionPool)
import I18N
import Persist
import Servant.Client hiding (Response)
import Symbols
import Telegram.Bot.API
import Telegram.Bot.Monadic
import Util

sendConfirmationRequests :: ConnectionPool -> LocalTime -> ClientM ()
sendConfirmationRequests pool now = do
  let tomorrow = succ $ localDay now
  slotsConfirmationRequestNotSent <-
    runInPool pool $ do
      selectFirst [OpenDayDate ==. tomorrow] [] >>= \case
        Nothing -> pure []
        Just (Entity tomorrow' _) -> do
          selectList
            [ScheduledSlotDay ==. tomorrow', ScheduledSlotConfirmed ==. Nothing]
            []
  forM_ slotsConfirmationRequestNotSent $ \(Entity slotId slot@ScheduledSlot {..}) -> do
    Just TelegramUser {..} <-
      runInPool pool $ do
        Just Volunteer {..} <- get scheduledSlotUser
        get volunteerUser
    let langs = maybeToList telegramUserLang
    slotDesc <- runInPool pool $ getSlotDesc langs slot
    void $
      sendMessage
        ( sendMessageRequest
            (ChatId (fromIntegral telegramUserUserId))
            (tr langs MsgConfirmTomorrow <> "\n" <> slotDesc)
        )
          { sendMessageParseMode = Just MarkdownV2,
            sendMessageReplyMarkup =
              Just $
                ik
                  [ [ ikb
                        (allGood <> tr langs MsgYesIWillCome)
                        ("confirm_" <> showSqlKey slotId)
                    ],
                    [ikb (tr langs MsgCantCome) ("cancel_" <> showSqlKey slotId)]
                  ]
          }

sendConfirmationReminders :: ConnectionPool -> LocalTime -> ClientM ()
sendConfirmationReminders pool now = do
  let tomorrow = succ $ localDay now
  slotsNotConfirmed <-
    runInPool pool $ do
      selectFirst [OpenDayDate ==. tomorrow] [] >>= \case
        Nothing -> pure []
        Just (Entity tomorrow' _) -> do
          selectList
            [ ScheduledSlotDay ==. tomorrow',
              ScheduledSlotConfirmed ==. Just False
            ]
            []
  forM_ slotsNotConfirmed $ \(Entity _ ScheduledSlot {..}) -> do
    Just TelegramUser {..} <-
      runInPool pool $ do
        Just Volunteer {..} <- get scheduledSlotUser
        get volunteerUser
    let langs = maybeToList telegramUserLang
    void $
      sendMessage
        ( sendMessageRequest
            (ChatId (fromIntegral telegramUserUserId))
            (attention <> tr langs MsgConfirmReminder)
        )
          { sendMessageParseMode = Just MarkdownV2
          }

notifyUnconfirmedSlots :: ConnectionPool -> LocalTime -> ClientM ()
notifyUnconfirmedSlots pool now = do
  let today = localDay now
  slotsNotConfirmed <-
    runInPool pool $ do
      selectFirst [OpenDayDate ==. today] [] >>= \case
        Nothing -> pure []
        Just (Entity today' _) -> do
          selectList
            [ ScheduledSlotDay ==. today',
              ScheduledSlotConfirmed ==. Just False,
              ScheduledSlotStartTime
                <=. localTimeOfDay (addLocalTime (2 * 60 * 60) now)
            ]
            []
  admins <- runInPool pool getAdmins
  forM_ slotsNotConfirmed $ \(Entity slotId slot) -> do
    forM_ admins $ \(TelegramUser auid lang _ _) -> do
      let langs = maybeToList lang
      slotFullDesc <- runInPool pool (getSlotFullDesc langs slot)
      void $
        sendMessage
          ( sendMessageRequest
              (ChatId (fromIntegral auid))
              (tr langs MsgSlotStillNotConfirmed <> "\n" <> slotFullDesc)
          )
            { sendMessageParseMode = Just MarkdownV2,
              sendMessageReplyMarkup =
                Just $
                  ik
                    [ [ ikb
                          (tr langs MsgSlotCancel)
                          ("cancel_" <> showSqlKey slotId)
                      ]
                    ]
            }

sendLastReminders :: ConnectionPool -> LocalTime -> ClientM ()
sendLastReminders pool now = do
  let today = localDay now
  slotsComingUp <-
    runInPool pool $ do
      selectFirst [OpenDayDate ==. today] [] >>= \case
        Nothing -> pure []
        Just (Entity today' _) -> do
          selectList
            [ ScheduledSlotDay ==. today',
              ScheduledSlotStartTime
                <=. localTimeOfDay (addLocalTime (60 * 60) now),
              ScheduledSlotReminderSent ==. False
            ]
            []
  forM_ slotsComingUp $ \(Entity _ ScheduledSlot {..}) -> do
    Just TelegramUser {..} <-
      runInPool pool $ do
        Just Volunteer {..} <- get scheduledSlotUser
        get volunteerUser
    let langs = maybeToList telegramUserLang
    void $
      sendMessage
        ( sendMessageRequest
            (ChatId (fromIntegral telegramUserUserId))
            (tr langs MsgLastReminder)
        )
          { sendMessageParseMode = Just MarkdownV2
          }

sendOpenDayReminder :: ConnectionPool -> LocalTime -> ClientM ()
sendOpenDayReminder pool now = do
  let today = localDay now
  runInPool pool (selectFirst [] [Desc OpenDayReminderSentOn]) >>= \case
    Just (Entity _ OpenDayReminder {..})
      | openDayReminderSentOn == today -> pure ()
    _ -> do
      admins <- runInPool pool getAdmins
      forM_ admins $ \(TelegramUser auid lang _ _) -> do
        let langs = maybeToList lang
        void $ runInPool pool $ insert $ OpenDayReminder today
        void $
          sendMessage
            ( sendMessageRequest
                (ChatId (fromIntegral auid))
                (news <> tr langs MsgSetOpenDaysReminder)
            )
              { sendMessageParseMode = Just MarkdownV2,
                sendMessageReplyMarkup =
                  Just $ ik [[ikb (tr langs MsgSetOpenDays) "setopendays"]]
              }

reminderBot :: ConnectionPool -> ClientM ()
reminderBot pool = do
  now <- zonedTimeToLocalTime <$> liftIO getZonedTime
  when (localTimeOfDay now >= TimeOfDay 12 00 00) $
    sendConfirmationRequests pool now
  when (localTimeOfDay now >= TimeOfDay 18 00 00) $
    sendConfirmationReminders pool now
  notifyUnconfirmedSlots pool now
  sendLastReminders pool now
  when
    ( dayOfWeek (localDay now) == Wednesday
        && localTimeOfDay now >= TimeOfDay 12 00 00
    )
    $ sendOpenDayReminder pool now
