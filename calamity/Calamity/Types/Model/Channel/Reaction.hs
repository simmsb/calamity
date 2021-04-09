-- | Message reactions
module Calamity.Types.Model.Channel.Reaction (Reaction (..)) where

import Calamity.Internal.AesonThings
import Calamity.Types.Model.Guild.Emoji

import Data.Aeson

import GHC.Generics

import TextShow
import qualified TextShow.Generic as TSG

data Reaction = Reaction
  { count :: Integer
  , me :: Bool
  , emoji :: RawEmoji
  }
  deriving (Eq, Show, Generic)
  deriving (TextShow) via TSG.FromGeneric Reaction
  deriving (ToJSON, FromJSON) via CalamityJSON Reaction
