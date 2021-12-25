{-# LANGUAGE TemplateHaskell #-}

-- | Command prefix parsing effect
module CalamityCommands.ParsePrefix (
  ParsePrefix (..),
  parsePrefix,
  useConstantPrefix,
) where

import qualified Data.Text as T

import qualified Polysemy as P

-- | An effect for parsing the prefix of a command.
data ParsePrefix msg m a where
  -- | Parse a prefix in a message, returning a tuple of @(prefix, remaining message)@
  ParsePrefix :: msg -> ParsePrefix msg m (Maybe (T.Text, T.Text))

P.makeSem ''ParsePrefix

-- | A default interpretation for 'ParsePrefix' that uses a single constant prefix.
useConstantPrefix :: T.Text -> P.Sem (ParsePrefix T.Text ': r) a -> P.Sem r a
useConstantPrefix pre =
  P.interpret
    ( \case
        ParsePrefix msg -> pure ((pre,) <$> T.stripPrefix pre msg)
    )
