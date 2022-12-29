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

attention :: Text
attention = "⚠ "

allGood :: Text
allGood = "✅ "

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

diamond :: Text
diamond = "🔶"
