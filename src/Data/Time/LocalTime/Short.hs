{-# LANGUAGE CPP, DerivingStrategies, DeriveAnyClass #-}
module Data.Time.LocalTime.Short
  ( ShortZonedTime
  , shortZonedTime
  , unShortZonedTime
  , ShortLocalTime
  , shortLocalTime
  , unShortLocalTime
  , FmtShortZonedTime
  , FmtShortLocalTime
  , arbitraryShortZonedTimeAfterDay
  , arbitraryShortLocalTimeAfterDay
  , module Data.Time.Fmt
  , shortZonedLocalDay
  , shortZonedLocalTimeOfDay
  , shortZonedTimeZoneString
  ) where

import           Control.DeepSeq (NFData)
import           Control.Lens
import           Control.Monad.State
import           Data.Aeson
import           Data.Binary (Binary)
import           Data.Binary.Instances.Time ()
import           Data.Coerce
import           Data.Ord (comparing)
import           Data.Time as T
import           Data.Time.Fmt
import           Data.Time.Utils
import           GHC.Generics (Generic)
import           Test.QuickCheck (Arbitrary)
import qualified Test.QuickCheck as QC
import           Test.QuickCheck.Instances ()

#if MIN_VERSION_time(1,9,0)
import           Data.Proxy
import           Data.Time.Format.Internal
#endif


-- | @ZonedTime@ where seconds = 0.
-- The constructor is not exported to maintain the invariant.
newtype ShortZonedTime
  = ShortZonedTime
  { unShortZonedTime :: T.ZonedTime
  } deriving stock (Generic)
  deriving newtype (FormatTime, NFData, FromJSON, ToJSON)

shortZonedLocalDay :: ShortZonedTime -> T.Day
shortZonedLocalDay = T.localDay . T.zonedTimeToLocalTime . unShortZonedTime

shortZonedLocalTimeOfDay :: ShortZonedTime -> T.TimeOfDay
shortZonedLocalTimeOfDay =
  T.localTimeOfDay . T.zonedTimeToLocalTime . unShortZonedTime

shortZonedTimeZoneString :: ShortZonedTime -> String
shortZonedTimeZoneString =
  T.timeZoneOffsetString . T.zonedTimeZone . unShortZonedTime

instance Binary ShortZonedTime

-- | Set seconds to 0.
clipZonedTime :: T.ZonedTime -> T.ZonedTime
clipZonedTime = set (_zonedTimeToLocalTime . _localTimeOfDay . _todSec) 0

-- | Smart constructor.
shortZonedTime :: T.ZonedTime -> ShortZonedTime
shortZonedTime = coerce . clipZonedTime

instance ParseTime ShortZonedTime where
  buildTime tl ps = shortZonedTime <$> buildTime tl ps
#if MIN_VERSION_time(1,9,0)
  substituteTimeSpecifier _ = substituteTimeSpecifier (Proxy @T.ZonedTime)
  parseTimeSpecifier _ = parseTimeSpecifier (Proxy @T.ZonedTime)
#endif

instance Show ShortZonedTime where
  -- Display as a string.
  showsPrec n = showsPrec n . show . unShortZonedTime

instance Eq ShortZonedTime where
  a == b = compare a b == EQ

instance Show ShortLocalTime where
  -- Display as a string.
  showsPrec n = showsPrec n . show . unShortLocalTime

-- | Fuck time zones.
instance Ord ShortZonedTime where
  compare a b = foldMap (\f -> f a b)
    [ comparing (T.zonedTimeToUTC . unShortZonedTime)
    , comparing (T.timeZoneMinutes . T.zonedTimeZone . unShortZonedTime) ]

-- | @LocalTime@ where seconds = 0.
-- The constructor is not exported to maintain the invariant.
newtype ShortLocalTime
  = ShortLocalTime
  { unShortLocalTime :: T.LocalTime
  } deriving stock (Eq, Ord, Generic)
  deriving newtype (FormatTime)
  deriving newtype (NFData)

instance Binary ShortLocalTime

-- | Set seconds to 0.
clipLocalTime :: T.LocalTime -> T.LocalTime
clipLocalTime = set (_localTimeOfDay . _todSec) 0

-- | Smart constructor.
shortLocalTime :: T.LocalTime -> ShortLocalTime
shortLocalTime = coerce . clipLocalTime

instance ParseTime ShortLocalTime where
  buildTime tl ps = shortLocalTime <$> buildTime tl ps
#if MIN_VERSION_time(1,9,0)
  substituteTimeSpecifier _ = substituteTimeSpecifier (Proxy @T.ZonedTime)
  parseTimeSpecifier _ = parseTimeSpecifier (Proxy @T.LocalTime)
#endif


type FmtShortZonedTime fmt = FmtTime fmt ShortZonedTime

type FmtShortLocalTime fmt = FmtTime fmt ShortLocalTime

-- | Fuck time zones. Really.
instance Arbitrary ShortZonedTime where
  arbitrary = evalStateT (arbitraryShortZonedTimeAfterDay 20000) 40000

-- | May be used to generate sequential dates.
--
-- @
-- [a,b,c] <- replicateM 3 (arbitraryShortZonedTimeAfterDay 5)
-- @
arbitraryShortZonedTimeAfterDay
  :: Integer -- ^ Day range
  -> StateT Integer QC.Gen ShortZonedTime
arbitraryShortZonedTimeAfterDay m = StateT $ \n -> do
  let day = T.ModifiedJulianDay n
  localTime <- QC.arbitrary <&> _localDay .~ day
  let zonedTime = T.ZonedTime localTime T.utc
  n' <- QC.choose (n, n + m)
  return (shortZonedTime zonedTime, n' + 1)

-- | May be used to generate sequential dates.
--
-- @
-- [a,b,c] <- replicateM 3 (arbitraryShortLocalTimeAfterDay 5)
-- @
arbitraryShortLocalTimeAfterDay
  :: Integer -- ^ Day range
  -> StateT Integer QC.Gen ShortLocalTime
arbitraryShortLocalTimeAfterDay m = StateT $ \n -> do
  let day = T.ModifiedJulianDay n
  localTime <- QC.arbitrary <&> _localDay .~ day
  n' <- QC.choose (n, n + m)
  return (shortLocalTime localTime, n' + 1)

instance Arbitrary ShortLocalTime where
  arbitrary = evalStateT (arbitraryShortLocalTimeAfterDay 20000) 40000
