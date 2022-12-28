{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Main where

import Bot (bot)
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (async)
import Control.Monad.Error.Class (MonadError (catchError))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (runNoLoggingT)
import Control.Monad.Reader as R
  ( forever,
    void,
  )
import Data.Text as T (pack)
import Database.Persist.Sqlite
  ( createSqlitePool,
  )
import Persist (migrate)
import ReminderBot (reminderBot)
import System.Environment ()
import System.Environment.Blank (getEnv, getEnvDefault)
import Telegram.Bot.API
  ( Token (Token),
    defaultRunBot,
  )
import Telegram.Bot.Monadic
  ( runTelegramIntegrationBot,
  )

main :: IO ()
main = do
  Just token <- getEnv "PARKI_AR_MINDA_TELEGRAM_TOKEN"
  database <- getEnvDefault "PARKI_AR_MINDA_DATABASE" "database.sqlite"
  pool <- runNoLoggingT $ createSqlitePool (pack database) 1000
  migrate pool
  let token' = Token (pack token)
  void $
    async $
      forever $ do
        void $
          async $
            catchError (void $ defaultRunBot token' $ reminderBot pool) print
        threadDelay $ 60 * 1000000
  void $ runTelegramIntegrationBot token' (\chat -> bot pool chat `catchError` (\e -> liftIO (print e) >> fail (show e)))
