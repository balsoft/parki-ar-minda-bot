{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module I18N
  ( BotMessage (..),
    tr,
    renderMessage,
    showDay,
  )
where

import Data.Text (Text, pack, replace)
import Data.Time
import Text.Shakespeare.I18N (Lang, RenderMessage (renderMessage), mkMessage, ToMessage (toMessage))
import Text.Blaze.Html (Html)
import Text.Blaze.Internal (MarkupM)
import Data.Text.Lazy (toStrict)
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Control.Monad (void)


data Bot
  = Bot

instance ToMessage (MarkupM a) where
  toMessage h = toStrict $ renderHtml (void h)

mkMessage "Bot" "messages" "en"

tr :: [Lang] -> BotMessage -> Text
tr langs = replace "\\n" "\n" . renderMessage Bot langs

data Month
  = January
  | February
  | March
  | April
  | May
  | June
  | July
  | August
  | September
  | October
  | November
  | December
  deriving (Show, Enum)

showDayOfWeek :: [Lang] -> DayOfWeek -> Text
showDayOfWeek langs d =
  tr langs $
    case d of
      Monday -> MsgMonday
      Tuesday -> MsgTuesday
      Wednesday -> MsgWednesday
      Thursday -> MsgThursday
      Friday -> MsgFriday
      Saturday -> MsgSaturday
      Sunday -> MsgSunday

showMonth :: [Lang] -> Month -> Text
showMonth langs m =
  tr langs $
    case m of
      January -> MsgJanuary
      February -> MsgFebruary
      March -> MsgMarch
      April -> MsgApril
      May -> MsgMay
      June -> MsgJune
      July -> MsgJuly
      August -> MsgAugust
      September -> MsgSeptember
      October -> MsgOctober
      November -> MsgNovember
      December -> MsgDecember

showDay :: [Lang] -> Day -> Text
showDay langs day =
  tr langs $
    MsgDate
      (showMonth langs (toEnum (month - 1)))
      (pack $ show dayOfMonth)
      (showDayOfWeek langs (dayOfWeek day))
  where
    (_year, month, dayOfMonth) = toGregorian day
