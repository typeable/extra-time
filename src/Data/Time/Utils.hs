module Data.Time.Utils
  ( the24h
  , oneHour
  , oneMinute
  , zoneLocalTime
  ) where

import Data.Time
import Data.Time.LocalTime.TimeZone.Series


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
