-- | Guilds that are unavailable
module Calamity.Types.Model.Guild.UnavailableGuild (UnavailableGuild (..)) where

import Calamity.Internal.AesonThings
import Calamity.Types.Model.Guild.Guild
import Calamity.Types.Snowflake

import Data.Aeson

import GHC.Generics

import TextShow
import qualified TextShow.Generic as TSG

data UnavailableGuild = UnavailableGuild
  { id :: Snowflake Guild
  , unavailable :: Bool
  }
  deriving (Eq, Show, Generic)
  deriving (TextShow) via TSG.FromGeneric UnavailableGuild
  deriving (ToJSON) via CalamityJSON UnavailableGuild
  deriving
    (FromJSON)
    via WithSpecialCases
          '["disabled" `IfNoneThen` DefaultToFalse]
          UnavailableGuild
  deriving (HasID Guild) via HasIDField "id" UnavailableGuild
