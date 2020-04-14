-- | Guilds that are unavailable
module Calamity.Types.Model.Guild.UnavailableGuild
    ( UnavailableGuild(..) ) where

import           Calamity.Internal.AesonThings
import           Calamity.Types.Model.Guild.Guild
import           Calamity.Types.Snowflake

import           Data.Aeson

data UnavailableGuild = UnavailableGuild
  { id          :: Snowflake Guild
  , unavailable :: Bool
  }
  deriving ( Eq, Show, Generic )
  deriving ( ToJSON, FromJSON ) via CalamityJSON UnavailableGuild
  deriving ( HasID ) via HasIDFieldAlt UnavailableGuild Guild
