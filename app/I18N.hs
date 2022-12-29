{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module I18N
  ( BotMessage (..),
    tr,
    renderMessage,
    escapeMd,
    showDay,
  )
where

import Data.Text (Text, pack, replace)
import Data.Time
import Text.Shakespeare.I18N (Lang, RenderMessage (renderMessage), mkMessage)

data Bot
  = Bot

mkMessage "Bot" "messages" "en"

tr :: [Lang] -> BotMessage -> Text
tr langs = replace "\\n" "\n" . escapeMd . renderMessage Bot langs

escapeMd :: Text -> Text
escapeMd txt =
  foldl
    (\s e -> replace e ("\\" <> e) s)
    txt
    ["~", ">", "#", "+", "-", "=", "|", "{", "}", ".", "!"]

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
