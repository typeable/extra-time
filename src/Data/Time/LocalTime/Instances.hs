{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.Time.LocalTime.Instances where

import           Data.Binary (Binary(..))
import           Data.Fixed (Fixed(..))
import           Data.Time
  (Day(..), LocalTime(..), TimeOfDay(..), TimeZone(..), ZonedTime(..))
import qualified Data.Time as T
import           Data.Time.Clock
import           GHC.Generics (Generic)


deriving instance Generic TimeZone

instance Binary TimeZone

deriving instance Generic Day

instance Binary Day

deriving instance Generic (Fixed a)

deriving instance Generic TimeOfDay

instance Binary TimeOfDay

deriving instance Generic LocalTime

instance Binary LocalTime

deriving instance Generic ZonedTime

instance Binary ZonedTime

instance Binary DiffTime where
  put = put . toRational
  get = fromRational <$> get

deriving instance Generic UTCTime

instance Binary UTCTime
