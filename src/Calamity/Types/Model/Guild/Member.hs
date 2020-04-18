-- | Guild Members
module Calamity.Types.Model.Guild.Member
    ( Member(..) ) where

import           Calamity.Internal.AesonThings
import {-# SOURCE #-} Calamity.Types.Model.Guild.Guild
import           Calamity.Types.Model.Guild.Role
import           Calamity.Types.Model.User
import           Calamity.Types.Snowflake

import           Data.Aeson
import           Data.Generics.Product.Fields
import           Data.Time
import           Data.Vector.Unboxed              ( Vector )

data Member = Member
  { user     :: User
  , guildID  :: Snowflake Guild
  , nick     :: Maybe ShortText
  , roles    :: Vector (Snowflake Role)
  , joinedAt :: UTCTime
  , deaf     :: Bool
  , mute     :: Bool
  }
  deriving ( Eq, Show, Generic )
  deriving ( ToJSON, FromJSON ) via CalamityJSON Member
  deriving ( HasID Guild ) via HasIDField "guildID" Member

instance HasID User Member where
  getID = getID . (^. field @"user")

instance HasID Member Member where
  getID = coerceSnowflake . getID @User
