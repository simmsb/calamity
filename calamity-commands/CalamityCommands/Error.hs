{-# LANGUAGE TemplateHaskell #-}

-- | Command errors
module CalamityCommands.Error (CommandError (..)) where

import Data.Text qualified as T
import TextShow.TH (deriveTextShow)

data CommandError
  = ParseError
      -- | The type of the parser
      T.Text
      -- | The reason that parsing failed
      T.Text
  | CheckError
      -- | The name of the check that failed
      T.Text
      -- | The reason for the check failing
      T.Text
  | InvokeError
      -- | The name of the command that failed
      T.Text
      -- | The reason for failing
      T.Text
  deriving (Show)

$(deriveTextShow ''CommandError)
