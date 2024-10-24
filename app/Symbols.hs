{-# LANGUAGE OverloadedStrings #-}

module Symbols where

import Data.Text

checkmark :: Text
checkmark = "✔"

checkIf :: Bool -> Text
checkIf b =
  if b
    then checkmark <> " "
    else ""

hello :: Text
hello = "👋"

party :: Text
party = "🥳"

locked :: Text
locked = "🔒"

unlocked :: Text
unlocked = "🔓"

attention :: Text
attention = "⚠"

allGood :: Text
allGood = "✅"

clock :: Text
clock = "⏰"

hourglass :: Text
hourglass = "⌛"

finished :: Text
finished = "🏁"

calendar :: Text
calendar = "📅"

house :: Text
house = "🏠"

forbidden :: Text
forbidden = "🚫"

bad :: Text
bad = "❌"

news :: Text
news = "⚡"

new :: Text
new = "➕"

info :: Text
info = "ⓘ "

change :: Text
change = "✍"

people :: Text
people = "👥"

person :: Text
person = "👤"

diamond :: Text
diamond = "🔶"

br :: Text
br = "&#13;&#10;"
