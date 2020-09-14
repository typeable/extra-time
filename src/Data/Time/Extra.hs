module Data.Time.Extra
  ( Minutes(..)
  , _Minutes
  , duration
  , durationLocal
  , minutesToNDF
  , UTCDate(..)
  , _UTCDate
  , UTCTime'(..)
  , _UTCTime'
  , mskTimeZone
  , toAPITimeMSK
  , apiDate
  , module Data.Time.LocalTime.Short
  , TimeFrame
  , mkTimeFrame
  , tfFrom
  , tfTo
  , tfType
  , TimeFrameType(..)
  , Hours(..)
  , _Hours
  , addUTCHours
  , DateRange(..)
  , mkDateRange
  , drFromDate
  , drToDate
  , module Data.Time.Utils
  ) where

import           Control.DeepSeq
import           Control.Lens
import           Control.Monad
import           Control.Monad.Fail
import           Data.Aeson
import           Data.Binary
import           Data.Function (on)
import qualified Data.Text as T
import           Data.Time
import           Data.Time.LocalTime.Short
import           Data.Time.Utils
import           GHC.Generics (Generic)
import           Test.QuickCheck
import           Test.QuickCheck.Arbitrary.Generic


newtype Minutes
  = Minutes { unMinutes :: Int
  } deriving (Show, Eq, Ord, Arbitrary, Generic, FromJSON, ToJSON)

instance NFData Minutes

makePrisms ''Minutes

newtype Hours
  = Hours { unHours :: Int
  } deriving (Show, Eq, Ord, Arbitrary, Generic, FromJSON, ToJSON)

instance NFData Hours

makePrisms ''Hours

addUTCHours :: Hours -> UTCTime -> UTCTime
addUTCHours (Hours h) t = addUTCTime s t
  where s = fromIntegral (h * 3600)

duration :: ShortZonedTime -> ShortZonedTime -> Minutes
duration t1 t2 = Minutes (timeDiff `div` 60)
  where
    calcDiff = diffUTCTime `on` zonedTimeToUTC . unShortZonedTime
    timeDiff = round (calcDiff t1 t2)

durationLocal :: LocalTime -> LocalTime -> Minutes
durationLocal t1 t2 = Minutes (timeDiff `div` 60)
  where
    calcDiff = diffUTCTime `on` localTimeToUTC utc
    timeDiff = round (calcDiff t1 t2)

minutesToNDF :: Minutes -> NominalDiffTime
minutesToNDF (Minutes mins) = 60 * realToFrac mins

-- UTCTime with lexically correct show instance.
-- Motivation: using ppShow.
newtype UTCTime' = UTCTime' { unUTCTime' :: UTCTime }
  deriving (Eq, Ord, Generic, Binary, NFData)

makePrisms ''UTCTime'

instance Show UTCTime' where
  showsPrec n
    = showsPrec n
    . formatTime defaultTimeLocale (iso8601DateFormat Nothing)
    . unUTCTime'

instance Arbitrary UTCTime' where
  arbitrary = UTCTime' <$> do
    UTCTime
      <$> (ModifiedJulianDay <$> choose (55000, 58000))
      <*> arbitrary

-- TODO: do not store the time, only the date
newtype UTCDate
  = UTCDate
  { utcTime :: UTCTime
  } deriving (Eq, Ord)

makePrisms ''UTCDate

instance NFData UTCDate where
  rnf (UTCDate d) = d `seq` () --  NOTE: not very good

instance Binary UTCDate where
  put (UTCDate t) = put d
    where
      UTCTime (ModifiedJulianDay d) _ = t
  get = do
    d <- get
    return $ UTCDate (UTCTime (ModifiedJulianDay d) 0)

instance Show UTCDate where
-- I hope no code was depending on this instance!
--  show = formatTime defaultTimeLocale "%d-%B-%y" . utcTime
  showsPrec n
    = showsPrec n
    . formatTime defaultTimeLocale "%d-%B-%y"
    . utcTime

instance ToJSON UTCDate where
  toJSON (UTCDate t) = String $ T.pack iso8601
    where iso8601 = formatTime defaultTimeLocale (iso8601DateFormat Nothing) t

instance FromJSON UTCDate where
  parseJSON (String s) = return $ UTCDate date
    where
      date   = parseTimeOrError True defaultTimeLocale format string
      format = iso8601DateFormat Nothing
      string = T.unpack s
  parseJSON _          = mzero

instance Arbitrary UTCDate where
  arbitrary = do
    dayNum <- choose (55000, 58000)
    return(UTCDate $ UTCTime (ModifiedJulianDay dayNum) (secondsToDiffTime 0))

toApiDate :: UTCDate -> T.Text
toApiDate (UTCDate t) = T.pack iso8601
  where
    iso8601 = formatTime defaultTimeLocale (iso8601DateFormat Nothing) t

fromApiDate :: (MonadFail m) => T.Text -> m UTCDate
fromApiDate t = UTCDate <$> parseTimeM
  True
  defaultTimeLocale
  (iso8601DateFormat Nothing)
  (T.unpack t)

apiDate :: Prism' T.Text UTCDate
apiDate = prism' toApiDate fromApiDate

mskTimeZone :: TimeZone
mskTimeZone = TimeZone (3 * 60) False "MSK"

toAPITimeMSK :: UTCTime -> ShortZonedTime
toAPITimeMSK = shortZonedTime . utcToZonedTime mskTimeZone

data TimeFrameType
  = TimeOfArrival
  | TimeOfDeparture
  deriving (Eq, Show, Generic)

instance Binary TimeFrameType

instance Arbitrary TimeFrameType where
  arbitrary = genericArbitrary
  shrink = genericShrink

data TimeFrame = TimeFrame
  { _tfFrom :: !TimeOfDay
  , _tfTo   :: !TimeOfDay
  , _tfType :: !TimeFrameType
  }
  deriving (Eq, Show, Generic)

makeLenses ''TimeFrame

instance Binary TimeFrame

instance Arbitrary TimeFrame where
  arbitrary = genericArbitrary
  shrink = genericShrink

mkTimeFrame :: TimeOfDay -> TimeOfDay -> TimeFrameType -> Maybe TimeFrame
mkTimeFrame fromTof toTof tft = if toTof >= fromTof
  then Just $ TimeFrame fromTof toTof tft
  else Nothing

data DateRange = DateRange
  { _drFromDate :: Day
  , _drToDate   :: Day
  } deriving (Eq, Show, Read, Generic)

makeLenses ''DateRange

mkDateRange :: Day -> Day -> DateRange
mkDateRange a b | b < a = DateRange b a
mkDateRange a b = DateRange a b

instance Arbitrary DateRange where
  arbitrary = mkDateRange <$> arbitrary <*> arbitrary
  shrink = genericShrink

instance Binary DateRange
