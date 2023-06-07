{-# LANGUAGE OverloadedStrings #-}

module Symbols where

import Data.Text

checkmark :: Text
checkmark = "✔ "

checkIf :: Bool -> Text
checkIf b =
  if b
    then checkmark
    else ""

hello :: Text
hello = "👋 "

party :: Text
party = "🥳 "

attention :: Text
attention = "⚠ "

allGood :: Text
allGood = "✅ "

clock :: Text
clock = "🕑 "

calendar :: Text
calendar = "📅 "

house :: Text
house = "🏠 "

forbidden :: Text
forbidden = "🚫 "

bad :: Text
bad = "❌ "

news :: Text
news = "⚡ "

info :: Text
info = "ⓘ "

people :: Text
people = "👥"

person :: Text
person = "👤"

diamond :: Text
diamond = "🔶"
