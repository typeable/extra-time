{-# OPTIONS -Wno-orphans #-}
{-# LANGUAGE DeriveLift #-}

module Holidays.EDay (EDay, eDayToDay) where

import Data.ByteString.Char8
import Data.Csv
import Data.Time
import Data.Time.Calendar.OrdinalDate
import qualified Data.Vector as V
import Language.Haskell.TH.Lift

data EDay = EDay {edYear :: Integer, edDay :: Int}
  deriving stock (Eq,Ord,Show,Lift)

eDayToDay :: EDay -> Day
eDayToDay (EDay y d) = fromOrdinalDate y  d

instance FromField EDay where
  parseField f = mkEDay <$> parseTimeM False defaultTimeLocale (iso8601DateFormat Nothing) (unpack f)
    where mkEDay d = let (theYear,theDay) = toOrdinalDate d in EDay theYear theDay

instance FromRecord EDay where
  parseRecord v
      | n == 1 = unsafeIndex v 0
      | otherwise = fail "Expected single field for a day"
        where
          n = V.length v
