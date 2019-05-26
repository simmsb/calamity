-- | Prelude for this library
{-# OPTIONS_GHC -Wno-orphans #-}

module Prelude
    ( module Protolude
    , module Control.Lens
    , module System.Log.Simple
    , module Fmt
    , module Data.Aeson.Lens
    , module Control.Arrow
    , module Data.Text.Short
    , LogMessage
    , CalamityJSON(..)
    , debug
    , info
    , warning
    , error
    , fatal
    , trace
    , jsonOptions
    , jsonOptionsKeepNothing
    , whenJust ) where

import           Control.Arrow     ( (>>>) )
import           Control.Lens      hiding ( (.=), (<.>), Level, Strict, from, op, to, uncons, unsnoc )

import           Data.Aeson        hiding ( Error )
import           Data.Aeson.Lens
import qualified Data.Text.Short   as ST
import           Data.Text.Short   ( ShortText )

import           Fmt

import           Protolude         hiding ( HasField, getField, trace )

import           System.Log.Simple hiding ( Debug, Error, Fatal, Info, Message, Trace, Warning, trace )
import qualified System.Log.Simple as SLS

type LogMessage = SLS.Message

debug, info, warning, error, fatal, trace :: MonadLog m => Text -> m ()
debug = sendLog SLS.Debug

info = sendLog SLS.Info

warning = sendLog SLS.Warning

error = sendLog SLS.Error

fatal = sendLog SLS.Fatal

trace = sendLog SLS.Trace

newtype CalamityJSON a = CalamityJSON
  { unCalamityJSON :: a
  }

instance (Typeable a, Generic a, GToJSON Zero (Rep a), GToEncoding Zero (Rep a)) => ToJSON (CalamityJSON a) where
  toJSON = genericToJSON jsonOptions . unCalamityJSON

  toEncoding = genericToEncoding jsonOptions . unCalamityJSON

instance (Typeable a, Generic a, GFromJSON Zero (Rep a)) => FromJSON (CalamityJSON a) where
  parseJSON = fmap CalamityJSON . genericParseJSON jsonOptions

jsonOptions :: Options
jsonOptions = defaultOptions { sumEncoding        = UntaggedValue
                             , fieldLabelModifier = camelTo2 '_' . filter (/= '_')
                             , omitNothingFields  = True }

jsonOptionsKeepNothing :: Options
jsonOptionsKeepNothing = defaultOptions { sumEncoding        = UntaggedValue
                                        , fieldLabelModifier = camelTo2 '_' . filter (/= '_')
                                        , omitNothingFields  = False }

whenJust :: Applicative m => Maybe a -> (a -> m ()) -> m ()
whenJust = flip $ maybe (pure ())

instance FromJSON ShortText where
  parseJSON = withText "Short Text" $ pure . ST.fromText

instance ToJSON ShortText where
  toJSON = toJSON . ST.toText

  toEncoding = toEncoding . ST.toText
