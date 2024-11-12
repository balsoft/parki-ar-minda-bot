{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module AppIntegration where

import Control.Monad (when, forM_, void)
import Data.Aeson ( ToJSON, encode )
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.UTF8 as BSU
import Data.Text ( pack, toLower, unpack, Text )
import Data.Time (Day (ModifiedJulianDay))
import Data.Time.Calendar (showGregorian)
import Data.Time.Clock
    ( diffUTCTime, getCurrentTime, UTCTime(UTCTime) )
import GHC.Generics (Generic)
import Network.HTTP.Client (Request (..), RequestBody (RequestBodyLBS), Response (responseStatus), httpLbs, newManager, parseRequest)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types.Status (Status (..))
import Util ( ScheduleList, showHourMinutes )
import Web.JWT
    ( encodeSigned,
      hmacSecret,
      numericDate,
      Algorithm(HS256),
      JOSEHeader(alg),
      JWTClaimsSet(iat) )
import Control.Monad.Except (MonadError(catchError))
import System.IO (hPrint)
import GHC.IO.Handle.FD (stderr)
import Data.Text.IO (hPutStrLn)

-- | Make a JWT signature for authorization
mkSignature :: Text -> UTCTime -> Text
mkSignature secret date = encodeSigned key jh cs
  where
    cs =
      mempty
        { iat = numericDate $ diffUTCTime date (UTCTime (ModifiedJulianDay 0) 0)
        }
    jh =
      mempty
        { alg = Just HS256
        }
    key = hmacSecret secret

-- | A single "shift" (actually just a period of time)
data AppScheduleShift = AppScheduleShift
  { start :: Text,
    end :: Text
  }
  deriving (Generic, ToJSON)

-- | Schedule for a day
data AppScheduleDay = AppScheduleDay
  { garageInnerFixedId :: Text,
    day :: Text,
    shifts :: [AppScheduleShift]
  }
  deriving (Generic, ToJSON)

type AppSchedule = [AppScheduleDay]

-- | Make a schedule to send
mkAppSchedule :: Text -> ScheduleList -> AppSchedule
mkAppSchedule garageName scheduleList =
  [ AppScheduleDay
      { garageInnerFixedId = toLower garageName,
        day = pack $ showGregorian day,
        shifts =
          [ AppScheduleShift
              { start = showHourMinutes start,
                end = showHourMinutes end
              }
            | (start, end) <- shifts
          ]
      }
    | (day, shifts) <- scheduleList
  ]

-- | Configuration for the app integration
data AppConfig = AppConfig { url :: Text, token :: Text }

-- a.k.a sequenceWhile
takeWhileM :: Monad m => (a -> Bool) -> [m a] -> m [a]
takeWhileM f (m:ms) = do
  a <- m
  if f a
    then do
      b <- takeWhileM f ms
      pure (a:b)
     else pure []
takeWhileM f [] = pure []

-- | Submit the schedule, handling authorization
submitSchedule :: AppConfig -> AppSchedule -> IO ()
submitSchedule AppConfig {..} schedule = do
  jwt <- mkSignature token <$> getCurrentTime
  req <- Network.HTTP.Client.parseRequest $ unpack url

  forM_ schedule $ \daySchedule -> flip catchError (hPrint stderr)
    (void $ takeWhileM not $ replicate 3 $ do
      let req' =
            req
              { method = "POST",
                requestBody = Network.HTTP.Client.RequestBodyLBS $ encode daySchedule,
                requestHeaders =
                  [ ("Authorization", "Bearer " <> BSU.fromString (unpack jwt)),
                    ("Content-Type", "application/json"),
                    ("ngrok-skip-browser-warning", "true")
                  ]
              }
      manager <- Network.HTTP.Client.newManager tlsManagerSettings
      resp <- Network.HTTP.Client.httpLbs req' manager

      if statusCode (Network.HTTP.Client.responseStatus resp) /= 200
        then do
          hPutStrLn stderr "Could not submit a schedule to the app"
          BSL.hPutStr stderr $ encode daySchedule
          hPutStrLn stderr ""
          hPrint stderr resp
          pure False
        else pure True
    )
