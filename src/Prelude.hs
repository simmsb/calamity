-- | Prelude for this library
{-# OPTIONS_GHC -Wno-orphans #-}

module Prelude
    ( module Protolude
    , module Control.Lens
    , module Fmt
    , module Data.Aeson.Lens
    , module Control.Arrow
    , module Data.Text.Short
    , module Data.Semigroup
    , module Calamity.Types.AesonThings
    , module Calamity.LogEff
    , module DiPolysemy
    , whenJust
    , lastMaybe ) where

import           Calamity.LogEff
import           Calamity.Types.AesonThings

import           Control.Arrow              ( (>>>) )
import           Control.Lens               hiding ( (.=), (<.>), Level, Strict, from, op, to, uncons, unsnoc )

import           Data.Aeson                 hiding ( Error )
import           Data.Aeson.Lens
import           Data.Semigroup             ( Last(..) )
import qualified Data.Text.Short            as ST
import           Data.Text.Short            ( ShortText )

import           DiPolysemy

import           Fmt

import           Protolude                  hiding ( HasField, Last, getField, getLast, trace, log )

whenJust :: Applicative m => Maybe a -> (a -> m ()) -> m ()
whenJust = flip $ maybe (pure ())

instance FromJSON ShortText where
  parseJSON = withText "Short Text" $ pure . ST.fromText

instance ToJSON ShortText where
  toJSON = toJSON . ST.toText

  toEncoding = toEncoding . ST.toText

lastMaybe :: Maybe a -> Maybe a -> Maybe a
lastMaybe l r = getLast <$> fmap Last l <> fmap Last r
