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
import Control.Concurrent ( threadDelay, newChan )
import Control.Concurrent.Async (async)
import Control.Monad.Error.Class (MonadError (catchError))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (runNoLoggingT)
import Control.Monad (forever, void)
import Data.Text as T (pack)
import Database.Persist.Sqlite (createSqlitePool)
import Persist (migrate)
import AppIntegration (AppConfig (AppConfig), submitSchedule)
import ReminderBot (reminderBot)
import System.Environment ()
import System.Environment.Blank (getEnv, getEnvDefault)
import Telegram.Bot.API (Token (Token), defaultRunBot)
import Telegram.Bot.Monadic (runTelegramIntegrationBot)
import Control.Monad (forM)
import Control.Concurrent.Chan (readChan)
import System.IO (hPrint)
import GHC.IO.Handle.FD (stderr)

printError :: (Show a) => Either a b -> IO ()
printError (Right _) = pure ()
printError (Left a) = hPrint stderr a

main :: IO ()
main = do
  Just token <- getEnv "PARKI_AR_MINDA_TELEGRAM_TOKEN"
  database <- getEnvDefault "PARKI_AR_MINDA_DATABASE" "database.sqlite"

  appUrl <- getEnv "PARKI_AR_MINDA_APP_URL"
  appToken <- getEnv "PARKI_AR_MINDA_APP_TOKEN"

  let appConfig = case (appUrl, appToken) of
        (Just a, Just t) -> Just $ AppConfig (pack a) (pack t)
        _ -> Nothing

  pool <- runNoLoggingT $ createSqlitePool (pack database) 1000
  migrate pool
  let token' = Token (pack token)
  void $
    async $
      forever $ do
        void $
          async $
            catchError (printError =<< defaultRunBot token' (reminderBot pool)) (hPrint stderr)
        threadDelay $ 60 * 1000000

  appChannel <- forM appConfig $ \config -> do
    chan <- newChan
    void $
      async $
        forever $ do
          appSchedule <- readChan chan
          submitSchedule config appSchedule
    pure chan


  void $
    runTelegramIntegrationBot
      token'
      ( \chat ->
          bot pool chat appChannel `catchError` (\e -> liftIO (hPrint stderr e) >> fail (show e))
      )
