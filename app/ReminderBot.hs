{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module ReminderBot
  ( reminderBot,
  )
where

import Control.Monad (forM_, void, when)
import Control.Monad.IO.Unlift (MonadIO (liftIO))
import Data.Maybe (maybeToList)
import Data.Text (pack)
import qualified Data.Text as T
import Data.Time
import Database.Persist
import Database.Persist.Sqlite (ConnectionPool)
import I18N
import Persist
import Servant.Client hiding (Response)
import Symbols
import Telegram.Bot.API
import Telegram.Bot.Monadic
import Text.Hamlet
import Util

sendConfirmationRequests :: ConnectionPool -> LocalTime -> ClientM ()
sendConfirmationRequests pool now = do
  let tomorrow = succ $ localDay now
  slotsConfirmationRequestNotSent <-
    runInPool pool $ do
      selectList [OpenDayDate ==. tomorrow] [] >>= \entities -> do
        selectList
          [ScheduledSlotDay <-. fmap entityKey entities, ScheduledSlotState ==. ScheduledSlotCreated]
          []
  forM_ slotsConfirmationRequestNotSent $ \(Entity slotId slot@ScheduledSlot {..}) -> do
    runInPool pool $ update slotId [ScheduledSlotState =. ScheduledSlotAwaitingConfirmation False]
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
            ( defaultRender
                langs
                [ihamlet|
                      _{MsgConfirmTomorrow}
                      #{slotDesc}
                    |]
            )
        )
          { sendMessageParseMode = Just HTML,
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
      selectList [OpenDayDate ==. tomorrow] [] >>= \entities -> do
        selectList
          [ ScheduledSlotDay <-. fmap entityKey entities,
            ScheduledSlotState ==. ScheduledSlotAwaitingConfirmation False
          ]
          []
  forM_ slotsNotConfirmed $ \(Entity slotId ScheduledSlot {..}) -> do
    Just TelegramUser {..} <-
      runInPool pool $ do
        Just Volunteer {..} <- get scheduledSlotUser
        update slotId [ScheduledSlotState =. ScheduledSlotAwaitingConfirmation True]
        get volunteerUser
    let langs = maybeToList telegramUserLang
    void $
      sendMessage
        ( sendMessageRequest
            (ChatId (fromIntegral telegramUserUserId))
            (attention <> tr langs MsgConfirmReminder)
        )
          { sendMessageParseMode = Just HTML
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
              ScheduledSlotState <-. (ScheduledSlotAwaitingConfirmation <$> [True, False]),
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
              ( defaultRender
                  langs
                  [ihamlet|
                _{MsgSlotStillNotConfirmed}
                #{slotFullDesc}
              |]
              )
          )
            { sendMessageParseMode = Just HTML,
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
      selectList [OpenDayDate ==. today] []
        >>= \entities -> do
          selectList
            [ ScheduledSlotDay <-. fmap entityKey entities,
              ScheduledSlotStartTime
                <=. localTimeOfDay (addLocalTime (60 * 60) now),
              ScheduledSlotReminderSent ==. False
            ]
            []
  forM_ slotsComingUp $ \(Entity slotId ScheduledSlot {..}) -> do
    Just TelegramUser {..} <-
      runInPool pool $ do
        Just Volunteer {..} <- get scheduledSlotUser
        update slotId [ScheduledSlotReminderSent =. True]
        get volunteerUser
    let langs = maybeToList telegramUserLang
    void $
      sendMessage
        ( sendMessageRequest
            (ChatId (fromIntegral telegramUserUserId))
            (tr langs MsgLastReminder)
        )
          { sendMessageParseMode = Just HTML
          }

sendChecklist :: ConnectionPool -> LocalTime -> ClientM ()
sendChecklist pool now = do
  let today = localDay now
  slotsFinished <-
    runInPool pool $ do
      selectList [OpenDayDate ==. today] []
        >>= \entities -> do
          selectList
            [ ScheduledSlotDay <-. fmap entityKey entities,
              ScheduledSlotStartTime
                <=. localTimeOfDay (addLocalTime (60 * 60) now),
              ScheduledSlotState ==. ScheduledSlotConfirmed
            ]
            []
  forM_ slotsFinished $ \(Entity slotId ScheduledSlot {..}) -> do
    Just TelegramUser {..} <-
      runInPool pool $ do
        Just Volunteer {..} <- get scheduledSlotUser
        get volunteerUser
    let langs = maybeToList telegramUserLang
    MessageId mid <-
      messageMessageId . responseResult
        <$> sendMessage
          ( sendMessageRequest
              (ChatId (fromIntegral telegramUserUserId))
              (tr langs MsgSlotFinished <> "\n\n" <> tr langs MsgChecklist <> "\n\n" <> T.intercalate "\n" (fmap (const "• " <> tr langs) [MsgCheckBags, MsgTakeTrashOut, MsgCloseTheDoor, MsgReturnKey, MsgReportVisitors]))
          )
            { sendMessageParseMode = Just HTML,
              sendMessageReplyMarkup = Just $ SomeForceReply $ ForceReply True (Just $ tr langs MsgNumber) Nothing
            }
    runInPool pool $
      update slotId [ScheduledSlotState =. ScheduledSlotFinished (fromIntegral mid)]

sendOpenDayReminder :: ConnectionPool -> LocalTime -> ClientM ()
sendOpenDayReminder pool now = do
  let today = localDay now
  garages <- runInPool pool (selectList [] [])
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
                (news <> tr langs (MsgSetOpenDaysReminder (showDay langs (nextWeekStart today))))
            )
              { sendMessageParseMode = Just HTML,
                sendMessageReplyMarkup =
                  Just $ ik [[ikb (tr langs (MsgSetOpenDays $ renderGarageText garage)) ("setopendays_" <> showSqlKey gid <> "_" <> pack (showGregorian (nextWeekStart today)))] | (Entity gid garage) <- garages]
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
  sendChecklist pool now
  when
    ( dayOfWeek (localDay now) == Wednesday
        && localTimeOfDay now >= TimeOfDay 12 00 00
    )
    $ sendOpenDayReminder pool now
