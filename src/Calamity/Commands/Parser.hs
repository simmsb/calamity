-- | Something that can parse user input
module Calamity.Commands.Parser
    ( Parser(..) ) where

import           Calamity.Commands.Context

import           Data.Kind
import           Data.Text.Lazy            ( Text )

class Parser (a :: Type) where
  type ParserResult a

  type ParserResult a = a

  parse :: (Context, Text) -> Either Text (ParserResult a, Text)

instance Parser Text where
  parse (_ctx, msg) = Right ("", msg)
