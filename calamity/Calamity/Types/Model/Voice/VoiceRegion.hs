-- | Voice regions
module Calamity.Types.Model.Voice.VoiceRegion (VoiceRegion (..)) where

import Calamity.Internal.AesonThings
import Calamity.Types.Snowflake

import Data.Aeson
import Data.Text (Text)

import GHC.Generics

import TextShow
import qualified TextShow.Generic as TSG

data VoiceRegion = VoiceRegion
  { id :: Snowflake VoiceRegion
  , name :: Text
  , vip :: Bool
  , optimal :: Bool
  , deprecated :: Bool
  , custom :: Bool
  }
  deriving (Show, Eq, Generic)
  deriving (TextShow) via TSG.FromGeneric VoiceRegion
  deriving (ToJSON, FromJSON) via CalamityJSON VoiceRegion
