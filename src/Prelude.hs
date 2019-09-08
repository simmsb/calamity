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
    , module Data.Semigroup
    , module Calamity.Types.AesonThings
    , LogMessage
    , debug
    , info
    , warning
    , error
    , fatal
    , trace
    , whenJust
    , lastMaybe ) where

import           Control.Arrow     ( (>>>) )
import           Control.Lens      hiding ( (.=), (<.>), Level, Strict, from, op, to, uncons, unsnoc )

import           Data.Aeson        hiding ( Error )
import           Data.Aeson.Lens
import           Data.Semigroup    ( Last(..) )
import qualified Data.Text.Short   as ST
import           Data.Text.Short   ( ShortText )

import           Fmt

import           Protolude         hiding ( HasField, Last, getField, getLast, trace )

import           System.Log.Simple hiding ( Debug, Error, Fatal, Info, Message, Trace, Warning, trace )
import qualified System.Log.Simple as SLS

import           Calamity.Types.AesonThings

type LogMessage = SLS.Message

debug, info, warning, error, fatal, trace :: MonadLog m => Text -> m ()
debug = sendLog SLS.Debug

info = sendLog SLS.Info

warning = sendLog SLS.Warning

error = sendLog SLS.Error

fatal = sendLog SLS.Fatal

trace = sendLog SLS.Trace

whenJust :: Applicative m => Maybe a -> (a -> m ()) -> m ()
whenJust = flip $ maybe (pure ())

instance FromJSON ShortText where
  parseJSON = withText "Short Text" $ pure . ST.fromText

instance ToJSON ShortText where
  toJSON = toJSON . ST.toText

  toEncoding = toEncoding . ST.toText

lastMaybe :: Maybe a -> Maybe a -> Maybe a
lastMaybe l r = getLast <$> fmap Last l <> fmap Last r
