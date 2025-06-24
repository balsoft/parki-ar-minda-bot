{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module ReminderBot
  ( reminderBot,
    ReminderConfig (..),
  )
where

import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (maybeToList)
import Data.Text (pack)
import Data.Time
import Database.Persist
import Database.Persist.Sqlite (ConnectionPool)
import I18N
import Persist
import Servant.Client hiding (Response)
import Symbols
import Telegram.Bot.API
import Text.Hamlet
import Util

data ReminderConfig = ReminderConfig
  { requestsTime :: TimeOfDay,
    remindersTime :: TimeOfDay,
    openDayRemindersDay :: DayOfWeek,
    openDayRemindersTime :: TimeOfDay
  }

sendConfirmationRequests :: ConnectionPool -> BotConfig -> LocalTime -> ClientM ()
sendConfirmationRequests pool botConfig now = do
  let tomorrow = succ $ localDay now
  tomorrow' <- runInPool pool $ selectList [OpenDayDate ==. tomorrow] []
  slotsConfirmationRequestNotSent <-
    runInPool pool $ do
      selectList [OpenDayDate ==. tomorrow] [] >>= \entities -> do
        selectList
          [ScheduledSlotDay <-. fmap entityKey entities, ScheduledSlotState ==. ScheduledSlotCreated]
          []
  forM_ slotsConfirmationRequestNotSent $ \(Entity slotId slot@ScheduledSlot {..}) -> ignoreError $ do
    runInPool pool $ update slotId [ScheduledSlotState =. ScheduledSlotAwaitingConfirmation False]
    Just TelegramUser {..} <-
      runInPool pool $ do
        Just Volunteer {..} <- get scheduledSlotUser
        get volunteerUser
    let langs = maybeToList telegramUserLang
    slotDesc <- runInPool pool $ getSlotDesc slot
    void $
      sendWithButtons
        (ChatId (fromIntegral telegramUserUserId))
        langs
        [ihamlet|
            _{MsgConfirmTomorrow}
            ^{slotDesc}
          |]
        [ [ (__ MsgYesIWillCome, "confirm_" <> showSqlKey slotId)
          ],
          [(__ MsgCantCome, "cancel_" <> showSqlKey slotId)]
        ]
  unless (null slotsConfirmationRequestNotSent) $ forM_ tomorrow' $ updateWorkingScheduleForDay pool botConfig False . entityKey

sendConfirmationReminders :: ConnectionPool -> BotConfig -> LocalTime -> ClientM ()
sendConfirmationReminders pool botConfig now = do
  let tomorrow = succ $ localDay now
  tomorrow' <- runInPool pool $ selectList [OpenDayDate ==. tomorrow] []
  slotsNotConfirmed <-
    runInPool pool $ do
      selectList
        [ ScheduledSlotDay <-. fmap entityKey tomorrow',
          ScheduledSlotState ==. ScheduledSlotAwaitingConfirmation False
        ]
        []
  forM_ slotsNotConfirmed $ \(Entity slotId slot@ScheduledSlot {..}) -> ignoreError $ do
    Just TelegramUser {..} <-
      runInPool pool $ do
        Just Volunteer {..} <- get scheduledSlotUser
        update slotId [ScheduledSlotState =. ScheduledSlotAwaitingConfirmation True]
        get volunteerUser
    let langs = maybeToList telegramUserLang
    slotDesc <- runInPool pool $ getSlotDesc slot
    void $
      sendWithButtons
        (ChatId (fromIntegral telegramUserUserId))
        langs
        [ihamlet|
          #{attention} _{MsgConfirmReminder}
          ^{slotDesc}
          |]
        [ [ (__ MsgYesIWillCome, "confirm_" <> showSqlKey slotId)
          ],
          [(__ MsgCantCome, "cancel_" <> showSqlKey slotId)]
        ]

  unless (null slotsNotConfirmed) $ forM_ tomorrow' $ updateWorkingScheduleForDay pool botConfig False . entityKey

notifyUnconfirmedSlots :: ConnectionPool -> BotConfig -> LocalTime -> ClientM ()
notifyUnconfirmedSlots pool botConfig now = do
  let today = localDay now
  today' <- runInPool pool $ selectList [OpenDayDate ==. today] []
  slotsNotConfirmed <-
    runInPool pool $
      selectList
        [ ScheduledSlotDay <-. fmap entityKey today',
          ScheduledSlotState <-. (ScheduledSlotAwaitingConfirmation <$> [True, False]),
          ScheduledSlotStartTime
            <=. localTimeOfDay (addLocalTime (2 * 60 * 60) now)
        ]
        []
  admins <- runInPool pool getAdmins
  forM_ slotsNotConfirmed $ \(Entity slotId slot) -> ignoreError $ do
    forM_ admins $ \(TelegramUser auid lang _ _) -> ignoreError $ do
      let langs = maybeToList lang
      slotFullDesc <- runInPool pool (getSlotFullDesc slot)
      runInPool pool $ update slotId [ScheduledSlotState =. ScheduledSlotUnconfirmed]
      void $
        sendWithButtons
          (ChatId (fromIntegral auid))
          langs
          [ihamlet|
            #{attention} _{MsgSlotStillNotConfirmed}
            ^{slotFullDesc}
          |]
          [[(__ MsgSlotCancel, "admin_cancel_" <> showSqlKey slotId)]]
  unless (null slotsNotConfirmed) $ forM_ today' $ updateWorkingScheduleForDay pool botConfig False . entityKey

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
  forM_ slotsComingUp $ \(Entity slotId slot@ScheduledSlot {..}) -> ignoreError $ do
    Just TelegramUser {..} <-
      runInPool pool $ do
        Just Volunteer {..} <- get scheduledSlotUser
        update slotId [ScheduledSlotReminderSent =. True]
        get volunteerUser
    slotDesc <- runInPool pool $ getSlotDesc slot
    let langs = maybeToList telegramUserLang
    void $
      send
        (ChatId (fromIntegral telegramUserUserId))
        langs
        [ihamlet|
          _{MsgLastReminder}
          ^{slotDesc}
        |]

sendChecklist :: ConnectionPool -> BotConfig -> LocalTime -> ClientM ()
sendChecklist pool botConfig now = do
  let today = localDay now
  today' <- runInPool pool $ selectList [OpenDayDate ==. today] []
  slotsFinished <-
    runInPool pool $ do
      selectList
        [ ScheduledSlotDay <-. fmap entityKey today',
          ScheduledSlotEndTime <=. localTimeOfDay now,
          ScheduledSlotState ==. ScheduledSlotConfirmed
        ]
        []
  forM_ slotsFinished $ \(Entity slotId ScheduledSlot {..}) -> ignoreError $ do
    Just TelegramUser {..} <-
      runInPool pool $ do
        Just Volunteer {..} <- get scheduledSlotUser
        get volunteerUser
    let langs = maybeToList telegramUserLang
    MessageId mid <-
      messageMessageId . responseResult
        <$> sendMessage
          ( defSendMessage
              (SomeChatId $ ChatId $ fromIntegral telegramUserUserId)
              (defaultRender langs $(ihamletFile "templates/checklist.ihamlet"))
          )
            { sendMessageParseMode = Just HTML,
              sendMessageReplyMarkup = Just $ SomeForceReply $ ForceReply True (Just $ MsgNumber |-> langs) Nothing
            }
    runInPool pool $
      update slotId [ScheduledSlotState =. ScheduledSlotFinished (fromIntegral mid)]
  unless (null slotsFinished) $ forM_ today' $ updateWorkingScheduleForDay pool botConfig False . entityKey

sendOpenDayReminder :: ConnectionPool -> LocalTime -> ClientM ()
sendOpenDayReminder pool now = do
  let today = localDay now
  disabledGarages <- runInPool pool (selectList [] [])
  garages <- runInPool pool (selectList [GarageId /<-. fmap (disabledGarageGarage . entityVal) disabledGarages] [])
  runInPool pool (selectFirst [] [Desc OpenDayReminderSentOn]) >>= \case
    Just (Entity _ OpenDayReminder {..})
      | openDayReminderSentOn == today -> pure ()
    _ -> do
      admins <- runInPool pool getAdmins
      forM_ admins $ \(TelegramUser auid lang _ _) -> ignoreError $ do
        let langs = maybeToList lang
        void $ runInPool pool $ insert $ OpenDayReminder today
        void $
          sendWithButtons
            (ChatId (fromIntegral auid))
            langs
            [ihamlet|#{news} _{MsgSetOpenDaysReminder (showDate (nextWeekStart today))}|]
            [ [(__ $ renderGarageText garage, "admin_setopendays_" <> showSqlKey gid <> "_" <> pack (showGregorian (nextWeekStart today)))]
              | (Entity gid garage) <- garages
            ]

reminderBot :: ConnectionPool -> BotConfig -> ReminderConfig -> ClientM ()
reminderBot pool botConfig (ReminderConfig {..}) = do
  now <- zonedTimeToLocalTime <$> liftIO getZonedTime
  when (localTimeOfDay now >= requestsTime) $
    sendConfirmationRequests pool botConfig now
  when (localTimeOfDay now >= remindersTime) $
    sendConfirmationReminders pool botConfig now
  notifyUnconfirmedSlots pool botConfig now
  sendLastReminders pool now
  sendChecklist pool botConfig now
  when
    ( dayOfWeek (localDay now) == openDayRemindersDay
        && localTimeOfDay now >= openDayRemindersTime
    )
    $ sendOpenDayReminder pool now
