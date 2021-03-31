{-# LANGUAGE TemplateHaskell #-}

-- | Command prefix parsing effect
module Calamity.Commands.ParsePrefix
    ( ParsePrefix(..)
    , parsePrefix
    , useConstantPrefix ) where

import           Calamity.Types.Model.Channel.Message

import           Control.Lens

import qualified Data.Text.Lazy                       as L

import qualified Polysemy                             as P

-- | An effect for parsing the prefix of a command.
data ParsePrefix m a where
  -- | Parse a prefix in a message, returning a tuple of @(prefix, remaining message)@
  ParsePrefix :: Message -> ParsePrefix m (Maybe (L.Text, L.Text))

P.makeSem ''ParsePrefix

-- | A default interpretation for 'ParsePrefix' that uses a single constant prefix.
useConstantPrefix :: L.Text -> P.Sem (ParsePrefix ': r) a -> P.Sem r a
useConstantPrefix pre = P.interpret (\case
                                       ParsePrefix m -> pure ((pre, ) <$> L.stripPrefix pre (m ^. #content)))
