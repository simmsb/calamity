{-# LANGUAGE TemplateHaskell #-}

-- | Command prefix parsing effect
module CalamityCommands.ParsePrefix
    ( ParsePrefix(..)
    , parsePrefix
    , useConstantPrefix ) where

import qualified Data.Text.Lazy                       as L

import qualified Polysemy                             as P

-- | An effect for parsing the prefix of a command.
data ParsePrefix msg m a where
  -- | Parse a prefix in a message, returning a tuple of @(prefix, remaining message)@
  ParsePrefix :: msg -> ParsePrefix msg m (Maybe (L.Text, L.Text))

P.makeSem ''ParsePrefix

-- | A default interpretation for 'ParsePrefix' that uses a single constant prefix.
useConstantPrefix :: L.Text -> P.Sem (ParsePrefix L.Text ': r) a -> P.Sem r a
useConstantPrefix pre = P.interpret (\case
                                        ParsePrefix msg -> pure ((pre, ) <$> L.stripPrefix pre msg))
