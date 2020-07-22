module Data.Time.Fmt
  ( FmtTime(..)
  , _FmtTime
  , ZonedISO8601
  , ZonedISO8601NoMs
  , LocalISO8601
  , DateISO8601
  , TimeISO8601
  , fmtTimeToString
  , fmtTimeFromString
  ) where

import           Control.DeepSeq (NFData)
import           Control.Lens
import           Control.Monad
import           Data.Aeson (FromJSON(..), ToJSON(..))
import           Data.Binary (Binary)
import           Data.Proxy
import           Data.String
import qualified Data.Text as T
import           Data.Time
import           GHC.Generics (Generic)
import           GHC.TypeLits
import           Test.QuickCheck
import           Text.XML.DOM.Parser
import           Text.XML.Writer
import           Web.HttpApiData


newtype FmtTime (fmt :: Symbol) a = FmtTime
  { unFmtTime :: a
  } deriving (NFData, Eq, Ord, Generic, Binary, Arbitrary, Functor, FormatTime)

makePrisms ''FmtTime

fmtTimeToString
  :: forall a fmt
  . (FormatTime a, KnownSymbol fmt)
  => FmtTime fmt a
  -> String
fmtTimeToString =
  formatTime defaultTimeLocale (symbolVal (Proxy @fmt)) . unFmtTime

fmtTimeFromString
  :: forall a fmt m
  . (ParseTime a, KnownSymbol fmt, Monad m)
  => String
  -> m (FmtTime fmt a)
fmtTimeFromString =
  let fmt = symbolVal (Proxy @fmt)
  in pure . FmtTime <=< parseTimeM True defaultTimeLocale fmt

instance (FormatTime a, KnownSymbol fmt) => Show (FmtTime fmt a) where
  showsPrec n = showsPrec n . fmtTimeToString

instance (ParseTime a, KnownSymbol fmt) => IsString (FmtTime fmt a) where
  fromString =
    let fmt = symbolVal (Proxy @fmt)
    in FmtTime . parseTimeOrError True defaultTimeLocale fmt

instance (FormatTime a, KnownSymbol fmt) => ToJSON (FmtTime fmt a) where
  toJSON = toJSON . fmtTimeToString

instance (ParseTime a, KnownSymbol fmt) => FromJSON (FmtTime fmt a) where
  parseJSON = fmtTimeFromString <=< parseJSON

instance (FormatTime a, KnownSymbol fmt) => ToHttpApiData (FmtTime fmt a) where
  toUrlPiece = T.pack . fmtTimeToString

instance (ParseTime a, KnownSymbol fmt) => FromHttpApiData (FmtTime fmt a) where
  parseUrlPiece t = maybe err Right $ fmtTimeFromString $ T.unpack t
    where
      fmt = T.pack $ symbolVal (Proxy @fmt)
      err = Left $ "no parse " <> t <> " as " <> fmt

instance (FormatTime a, KnownSymbol fmt) => ToXML (FmtTime fmt a) where
  toXML = toXML . T.pack . fmtTimeToString

instance (ParseTime a, KnownSymbol fmt) => FromDom (FmtTime fmt a) where
  fromDom = do
    s <- fromDom
    case fmtTimeFromString s of
      Right a -> return a
      Left  e -> throwParserError $
        PEContentWrongFormat (T.pack e)

type ZonedISO8601 = ("%FT%T%Q%z" :: Symbol)

type ZonedISO8601NoMs = ("%FT%T%z" :: Symbol)

type LocalISO8601 = ("%FT%T%Q" :: Symbol)

type DateISO8601 = ("%F" :: Symbol)

type TimeISO8601 = ("%T" :: Symbol)
