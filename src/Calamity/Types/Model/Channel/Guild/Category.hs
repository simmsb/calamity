module Calamity.Types.Model.Channel.Guild.Category
    ( Category(..) ) where

import           Calamity.Internal.AesonThings
import           Calamity.Internal.SnowflakeMap
import {-# SOURCE #-} Calamity.Types.Model.Channel
import {-# SOURCE #-} Calamity.Types.Model.Guild.Guild
import           Calamity.Types.Model.Guild.Overwrite
import           Calamity.Types.Snowflake

import           Data.Aeson
import           Data.Vector                          ( Vector )

data Category = Category
  { id                   :: Snowflake Category
  , permissionOverwrites :: Vector Overwrite
  , name                 :: ShortText
  , nsfw                 :: Bool
  , position             :: Int
  , guildID              :: Snowflake Guild
  }
  deriving ( Show, Eq, Generic )
  deriving ( ToJSON ) via CalamityJSON Category
  deriving ( FromJSON ) via WithSpecialCases '[IfNoneThen "nsfw" DefaultToFalse] Category
  deriving ( HasID ) via HasIDFieldCoerce Category Channel
