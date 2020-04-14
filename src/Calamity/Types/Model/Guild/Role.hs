-- | Guild roles
module Calamity.Types.Model.Guild.Role
    ( Role(..) ) where

import           Calamity.Internal.AesonThings
import           Calamity.Types.Snowflake

import           Data.Aeson

data Role = Role
  { id          :: Snowflake Role
  , name        :: ShortText
  , color       :: Word64
  , hoist       :: Bool
  , position    :: Int
  , permissions :: Word64
  , managed     :: Bool
  , mentionable :: Bool
  }
  deriving ( Eq, Show, Generic )
  deriving ( ToJSON, FromJSON ) via CalamityJSON Role
  deriving ( HasID ) via HasIDField Role
