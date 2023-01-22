{-# LANGUAGE TemplateHaskell #-}

-- | Command errors
module CalamityCommands.Error (CommandError (..)) where

import Data.Text qualified as T
import TextShow.TH (deriveTextShow)

data CommandError
  = ParseError
      T.Text
      -- ^ The type of the parser
      T.Text
      -- ^ The reason that parsing failed
  | CheckError
      T.Text
      -- ^ The name of the check that failed
      T.Text
      -- ^ The reason for the check failing
  | InvokeError
      T.Text
      -- ^ The name of the command that failed
      T.Text
      -- ^ The reason for failing
  deriving (Show)

$(deriveTextShow ''CommandError)
