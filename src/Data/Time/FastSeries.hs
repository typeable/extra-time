module Data.Time.FastSeries
  ( TimeZoneSeries
  , toTimeZoneSeries
  , fromTimeZoneSeries
  , timeZoneFromSeries
  , timeZoneFromSeriesLocal
  , utcToLocalTime'
  , localTimeToUTC'
  ) where

import Data.Map.Strict (Map)
import qualified Data.Map as Map
import Data.Time
import qualified Data.Time.LocalTime.TimeZone.Series as Series


-- | Contains the same data as 'Series.TimeZoneSeries' but precomputes some data
-- for faster conversion.
data TimeZoneSeries = TimeZoneSeries
  { fallback :: !TimeZone
  , transitionsUTC :: !(Map UTCTime TimeZone)
    -- ^ UTC -> Local wants to know the last transition that happened before a
    -- given 'UTCTime'
  , transitionsLocal :: !(Map LocalTime TimeZone)
    -- ^ Local -> UTC wants to know the largest transition @t@ into timezone
    -- @tz@ such that @t <= localTimeToUTC tz lt@ where @lt@ is a given
    -- 'LocalTime'. For a given TZ and valid time arguments, 'localTimeToUTC'
    -- and 'utcToLocalTime' are inverses. Hence this is equivalent to
    -- @utcToLocalTime t tz <= lt@.
  }

toTimeZoneSeries :: Series.TimeZoneSeries -> TimeZoneSeries
toTimeZoneSeries
  Series.TimeZoneSeries { Series.tzsTimeZone, Series.tzsTransitions }
  = TimeZoneSeries
    { fallback = tzsTimeZone
    , transitionsUTC = Map.fromDescList tzsTransitions
    , transitionsLocal = Map.fromList
      [(utcToLocalTime tz u, tz) | (u, tz) <- tzsTransitions]
    }

fromTimeZoneSeries :: TimeZoneSeries -> Series.TimeZoneSeries
fromTimeZoneSeries TimeZoneSeries { fallback, transitionsUTC }
  = Series.TimeZoneSeries
    { Series.tzsTimeZone = fallback
    , Series.tzsTransitions = Map.toList transitionsUTC
    }

timeZoneFromSeries :: TimeZoneSeries -> UTCTime -> TimeZone
timeZoneFromSeries TimeZoneSeries { fallback, transitionsUTC } t
  = maybe fallback snd $ Map.lookupLE t transitionsUTC

-- | See caveat on 'localTimeToUTC''.
timeZoneFromSeriesLocal :: TimeZoneSeries -> LocalTime -> TimeZone
timeZoneFromSeriesLocal TimeZoneSeries { fallback, transitionsLocal } lt
  = maybe fallback snd $ Map.lookupLE lt transitionsLocal

utcToLocalTime' :: TimeZoneSeries -> UTCTime -> LocalTime
utcToLocalTime' tzsf t = utcToLocalTime tz t
  where !tz = timeZoneFromSeries tzsf t

-- | This conversion is not actually always well defined.
--
-- At the moment of switching to summer time, there is a range of 'LocalTime's
-- that are skipped. For these, 'Series.isValidLocalTime' returns 'False'. This
-- function converts them to 'UTCTime' as if the switch didn't happen.
--
-- Around the moment of switching away from summer time, there is a range of
-- 'LocalTime's that happen twice: first before the switch, then once again
-- after the switch. For these, 'Series.isRedundantLocalTime' returns 'True'.
-- In such ambiguous cases, this function always picks the occurrence after the
-- switch.
--
-- The handling of these corner cases matches exactly to that of
-- 'Series.localTimeToUTC''
--
-- This means the equation
-- @lt == utcToLocalTime' tzsf (localTimeToUTC' tzsf lt)@ does not hold for @lt@
-- that were skipped. And the equation
-- @t == localTimeToUTC' tzsf (utcToLocalTime' tzsf t)@ does not hold for @t@
-- that come just before switching awya from summer time. For other arguments
-- these equations are assumed to hold.
localTimeToUTC' :: TimeZoneSeries -> LocalTime -> UTCTime
localTimeToUTC' tzsf lt = localTimeToUTC tz lt
  where !tz = timeZoneFromSeriesLocal tzsf lt
