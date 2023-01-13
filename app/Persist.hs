{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Persist where

import Control.Monad.IO.Unlift (MonadIO (..), MonadUnliftIO (..))
import Control.Monad.Logger (NoLoggingT (..))
import Control.Monad.Trans.Reader (ReaderT (..))
import qualified Data.Pool as Pool
import Data.Text (Text, pack, unpack)
import Data.Time
import Database.Persist
import Database.Persist.SqlBackend.Internal (SqlBackend)
import Database.Persist.Sqlite
  ( BackendKey (SqlBackendKey),
    ConnectionPool,
    PersistFieldSql (..),
    createSqlitePool,
    fromSqlKey,
    runMigration,
    toSqlKey,
  )
import Database.Persist.TH
  ( mkMigrate,
    mkPersist,
    persistLowerCase,
    share,
    sqlSettings,
  )

instance PersistField DayOfWeek where
  toPersistValue = PersistText . pack . show
  fromPersistValue (PersistText t) = Right $ read $ unpack t
  fromPersistValue _ = undefined

instance PersistFieldSql DayOfWeek where
  sqlType _ = SqlString

data ScheduledSlotState
  = ScheduledSlotCreated
  | ScheduledSlotAwaitingConfirmation Bool
  | ScheduledSlotConfirmed
  | ScheduledSlotFinished { checklistMsg :: Int }
  | ScheduledSlotChecklistComplete { visitors :: Int }
  deriving (Show, Read)

instance PersistField ScheduledSlotState where
  toPersistValue = PersistText . pack . show
  fromPersistValue (PersistText t) = Right $ read $ unpack t
  fromPersistValue _ = undefined

instance PersistFieldSql ScheduledSlotState where
  sqlType _ = SqlString

runInPool :: MonadIO m => ConnectionPool -> ReaderT SqlBackend IO a -> m a
runInPool pool = liftIO . Pool.withResource pool . runReaderT

share
  [mkPersist sqlSettings, mkMigrate "migrateAll"]
  [persistLowerCase|
Garage
    name Text
    address Text
    UniqueGarage name
    deriving Show
TelegramUser
    userId Int
    lang Text Maybe
    fullName Text
    username Text Maybe
    UniqueTelegramUser userId
    deriving Show
Volunteer
    user TelegramUserId OnDeleteCascade OnUpdateCascade
    UniqueVolunteer user
    deriving Show
Admin
    user TelegramUserId
    UniqueAdmin user
    deriving Show
OpenDayReminder
    sentOn Day
    UniqueOpenDayReminder sentOn
    deriving Show
CallbackQueryMultiChat
    chatId Int
    msgId Int
    callbackQuery Text
    deriving Show
Subscription
    user TelegramUserId OnDeleteCascade OnUpdateCascade
    UniqueSubscription user
    deriving Show
DefaultOpenDay
    garage GarageId
    dayOfWeek DayOfWeek
    deriving Show
OpenDay
    garage GarageId
    date Day
    available Bool
    UniqueDay date garage
    deriving Show
ScheduledSlot
    day OpenDayId OnDeleteCascade OnUpdateCascade
    startTime TimeOfDay
    endTime TimeOfDay
    user VolunteerId OnUpdateCascade
    state ScheduledSlotState
    reminderSent Bool
    UniqueSlot day startTime endTime user
    deriving Show
|]

createPool :: MonadUnliftIO m => String -> m (Pool.Pool SqlBackend)
createPool database = runNoLoggingT $ createSqlitePool (pack database) 1000

showSqlKey :: (ToBackendKey SqlBackend a) => Key a -> Text
showSqlKey = pack . show . fromSqlKey

readSqlKey :: (ToBackendKey SqlBackend a) => Text -> Key a
readSqlKey = toSqlKey . read . unpack

migrate :: MonadIO m => ConnectionPool -> m ()
migrate pool = runInPool pool $ runMigration migrateAll
