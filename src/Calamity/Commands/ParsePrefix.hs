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

data ParsePrefix m a where
  ParsePrefix :: Message -> ParsePrefix m (Maybe (L.Text, L.Text))

P.makeSem ''ParsePrefix

useConstantPrefix :: L.Text -> P.Sem (ParsePrefix ': r) a -> P.Sem r a
useConstantPrefix pre = P.interpret (\case
                                       ParsePrefix m -> pure ((pre, ) <$> L.stripPrefix pre (m ^. #content)))
