-- | Voice channels
module Calamity.Types.Model.Channel.Guild.Voice
    ( VoiceChannel(..) ) where

import           Calamity.Internal.AesonThings
import {-# SOURCE #-} Calamity.Types.Model.Channel
import {-# SOURCE #-} Calamity.Types.Model.Channel.Guild.Category
import {-# SOURCE #-} Calamity.Types.Model.Channel.Message
import {-# SOURCE #-} Calamity.Types.Model.Guild.Guild
import           Calamity.Types.Model.Guild.Overwrite
import           Calamity.Types.Snowflake

import           Data.Aeson
import           Data.Vector                                 ( Vector )

data VoiceChannel = VoiceChannel
  { id                   :: Snowflake VoiceChannel
  , guildID              :: Snowflake Guild
  , position             :: Int
  , permissionOverwrites :: Vector Overwrite
  , name                 :: ShortText
  , bitrate              :: Int
  , userLimit            :: Int
  , parentID             :: Maybe (Snowflake Category)
  }
  deriving ( Show, Eq, Generic )
  deriving ( ToJSON, FromJSON ) via CalamityJSON VoiceChannel
  deriving ( HasID ) via HasIDFieldCoerce VoiceChannel Channel
