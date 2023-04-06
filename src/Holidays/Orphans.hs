{-# OPTIONS -Wno-orphans #-}

module Holidays.Orphans () where

import Data.ByteString.Char8
import Data.Csv
import Data.Time
import Language.Haskell.TH.Lift

deriveLift ''Day
deriveLift ''Only

instance FromField Day where
  parseField =
    parseTimeM False defaultTimeLocale (iso8601DateFormat Nothing) . unpack
