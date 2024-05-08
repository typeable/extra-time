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
import           Data.Time.FastSeries


the24h :: NominalDiffTime
the24h = 24 * oneHour

oneHour :: NominalDiffTime
oneHour = 60 * 60

oneMinute :: NominalDiffTime
oneMinute = 60

-- | Calculate 'TimeZone' using 'TimeZoneSeries' and construct 'ZonedTime'.
--
-- See caveats on 'localTimeToUTC''.
zoneLocalTime
  :: TimeZoneSeries
  -> LocalTime
  -> ZonedTime
zoneLocalTime tzsf lt = ZonedTime lt tz
  where !tz = timeZoneFromSeriesLocal tzsf lt

makeLensesFor
  [("zonedTimeToLocalTime", "_zonedTimeToLocalTime")] ''ZonedTime

makeLensesFor
  [("localDay", "_localDay"), ("localTimeOfDay", "_localTimeOfDay")]
  ''LocalTime

makeLensesFor
  [ ("todSec", "_todSec")
  , ("todMin", "_todMin")
  , ("todHour", "_todHour")] ''TimeOfDay
