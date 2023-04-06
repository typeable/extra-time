module Data.Time.Holidays
  ( addTimeExcludingHolidays
  , opmHolidays2023to2030
  ) where

import           Data.Proxy
import qualified Data.Set as S
import           Data.Time
import           Data.Time.Calendar.OrdinalDate
import           Data.Time.LocalTime.TimeZone.Series
import           Data.Csv (Only(..))
import           Data.FileEmbed (makeRelativeToProject)
import           Data.Csv.Embed (embedRecords)
import           Holidays.Orphans ()

opmHolidays2023to2030 :: S.Set Day
opmHolidays2023to2030 = S.fromList $
  fromOnly <$>
    $(makeRelativeToProject "data/opm-holidays.csv" >>= embedRecords (Proxy @(Only Day)))

addTimeExcludingHolidays
  :: S.Set Day
  -> TimeZoneSeries
  -> DiffTime
  -> UTCTime
  -> UTCTime
addTimeExcludingHolidays hDays tz dTime =
  addLocalTime' dTime . utcToLocalTime' tz
  where
    shouldSkip day = weekday `elem` [6, 7] || day `S.member` hDays
      where (_, weekday) = mondayStartWeek day
    dayPicoseconds =
      diffTimeToPicoseconds . secondsToDiffTime $ 60 * 60 * 24
    addLocalTime' :: DiffTime -> LocalTime -> UTCTime
    addLocalTime' dTime' (LocalTime lDay' lTime') =
      let
        actualDays = filter (not . shouldSkip) [lDay'..]
        -- Jumps to nearest day if `now` is a holiday
        lTime = if shouldSkip lDay'
          then midnight
          else lTime'
        -- At this point the head of `actualDays` and `lTime`
        -- is the day from which we should be counting
        (dDays, dPicoSeconds) =
          (diffTimeToPicoseconds $ dTime' + (timeOfDayToTime lTime))
            `divMod` dayPicoseconds
        resultDay = head . drop (fromIntegral dDays) $ actualDays
        resultTime =
          timeToTimeOfDay . picosecondsToDiffTime $ dPicoSeconds
      in localTimeToUTC' tz $ LocalTime resultDay resultTime
