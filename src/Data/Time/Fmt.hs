{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP #-}
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
import           Web.HttpApiData

#ifndef ghcjs_HOST_OS
import           Text.XML.DOM.Parser
import           Text.XML.Writer
#endif

#if !MIN_VERSION_base(4,13,0)
import           Control.Monad.Fail
#endif


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
  . (ParseTime a, KnownSymbol fmt, MonadFail m)
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

errParseFmtTime
  :: forall fmt s. (KnownSymbol fmt, IsString s, Semigroup s) => s -> s
errParseFmtTime t =
  "no parse " <> t <> " as " <> fromString (symbolVal (Proxy @fmt))

instance (ParseTime a, KnownSymbol fmt) => FromHttpApiData (FmtTime fmt a) where
  parseUrlPiece t =
    maybe (Left $ errParseFmtTime @fmt t) Right $ fmtTimeFromString $ T.unpack t

#ifndef ghcjs_HOST_OS
instance (FormatTime a, KnownSymbol fmt) => ToXML (FmtTime fmt a) where
  toXML = toXML . T.pack . fmtTimeToString

instance (ParseTime a, KnownSymbol fmt) => FromDom (FmtTime fmt a) where
  fromDom = do
    s <- fromDom
    maybe (throwParserError $ PEContentWrongFormat $ errParseFmtTime @fmt s)
      pure $ fmtTimeFromString $ T.unpack s
#endif

type ZonedISO8601 = ("%FT%T%Q%z" :: Symbol)

type ZonedISO8601NoMs = ("%FT%T%z" :: Symbol)

type LocalISO8601 = ("%FT%T%Q" :: Symbol)

type DateISO8601 = ("%F" :: Symbol)

type TimeISO8601 = ("%T" :: Symbol)
