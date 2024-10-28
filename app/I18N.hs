{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes #-}

module I18N
  ( BotMessage (..),
    renderMessage,
    showDate,
    IHamlet,
    defaultLayout,
    defaultRender,
    ToIHamlet (..),
    __,
    (|->)
  )
where

import Data.Text (Text, pack, replace)
import Data.Time (Day, DayOfWeek (..), dayOfWeek, toGregorian)
import Text.Shakespeare.I18N (Lang, RenderMessage (renderMessage), mkMessage, ToMessage (toMessage))
import Text.Blaze.Html (Html)
import Text.Blaze.Internal (MarkupM)
import Data.Text.Lazy (toStrict)
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Control.Monad (void)
import Text.Blaze (preEscapedText)
import Text.Hamlet (ihamlet)

data Bot
  = Bot

instance ToMessage (MarkupM a) where
  toMessage h = toStrict $ renderHtml (void h)
instance ToMessage Int where
  toMessage = pack . show

_tr :: RenderMessage Bot message => Lang -> message -> Text
_tr lang = renderMessage Bot [lang]

mkMessage "Bot" "messages" "en"

type IHamlet = (BotMessage -> Html) -> (() -> ()) -> Html

defaultLayout :: [Lang] -> IHamlet -> Html
defaultLayout langs f = f (preEscapedText <$> tr langs) (const ())

defaultRender :: [Lang] -> IHamlet -> Text
defaultRender langs = toStrict . renderHtml . defaultLayout langs

class ToIHamlet a where
  toIHamlet :: a -> IHamlet

__ :: ToIHamlet a => a -> IHamlet
__ = toIHamlet

instance ToIHamlet BotMessage where
  toIHamlet m = [ihamlet|_{m}|]

instance ToIHamlet Text where
  toIHamlet m = [ihamlet|#{m}|]

instance ToIHamlet Int where
  toIHamlet m = [ihamlet|#{m}|]

instance ToIHamlet Day where
  toIHamlet = toIHamlet . showDate

instance ToIHamlet Month where
  toIHamlet = toIHamlet . showMonth

instance ToIHamlet DayOfWeek where
  toIHamlet = toIHamlet . showDayOfWeek

tr :: [Lang] -> BotMessage -> Text
tr langs = replace "\\n" "\n" . renderMessage Bot langs

(|->) :: BotMessage -> [Lang] -> Text
(|->) = flip tr

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

showDayOfWeek :: DayOfWeek -> BotMessage
showDayOfWeek d =
  case d of
    Monday -> MsgMonday
    Tuesday -> MsgTuesday
    Wednesday -> MsgWednesday
    Thursday -> MsgThursday
    Friday -> MsgFriday
    Saturday -> MsgSaturday
    Sunday -> MsgSunday

showMonth :: Month -> BotMessage
showMonth m =
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

showDate :: Day -> BotMessage
showDate  day =
  MsgDate (showMonth (toEnum (month - 1))) dayOfMonth (showDayOfWeek (dayOfWeek day))
  where
    (_year, month, dayOfMonth) = toGregorian day
