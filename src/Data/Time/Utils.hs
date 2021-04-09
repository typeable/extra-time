module Data.Time.Utils
  ( the24h
  , oneHour
  , oneMinute
  , zoneLocalTime
  , _zonedTimeToLocalTime
  , _localDay
  , _localTimeOfDay
  , _todSec
  , _todMin
  , _todHour
  ) where

import           Control.Lens
import           Data.Time
import           Data.Time.LocalTime.TimeZone.Series


the24h :: NominalDiffTime
the24h = 24 * oneHour

oneHour :: NominalDiffTime
oneHour = 60 * 60

oneMinute :: NominalDiffTime
oneMinute = 60

-- | Calculate TimeZone using TimeZoneSeries and construct ZonedTime
zoneLocalTime
  :: TimeZoneSeries
  -> LocalTime
  -> ZonedTime
zoneLocalTime tzs lt = let
  u  = localTimeToUTC' tzs lt
  tz = timeZoneFromSeries tzs u
  in ZonedTime lt tz

makeLensesFor
  [("zonedTimeToLocalTime", "_zonedTimeToLocalTime")] ''ZonedTime

makeLensesFor
  [("localDay", "_localDay"), ("localTimeOfDay", "_localTimeOfDay")]
  ''LocalTime

makeLensesFor
  [ ("todSec", "_todSec")
  , ("todMin", "_todMin")
  , ("todHour", "_todHour")] ''TimeOfDay
