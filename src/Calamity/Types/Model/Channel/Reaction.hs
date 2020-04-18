-- | Message reactions
module Calamity.Types.Model.Channel.Reaction
    ( Reaction(..) ) where

import           Calamity.Internal.AesonThings
import {-# SOURCE #-} Calamity.Types.Model.Channel
import {-# SOURCE #-} Calamity.Types.Model.Channel.Message
import           Calamity.Types.Model.Guild.Emoji
import {-# SOURCE #-} Calamity.Types.Model.Guild.Guild
import           Calamity.Types.Model.User
import           Calamity.Types.Snowflake

import           Data.Aeson

data Reaction = Reaction
  { userID    :: Snowflake User
  , channelID :: Snowflake Channel
  , messageID :: Snowflake Message
  , guildID   :: Maybe (Snowflake Guild)
  , emoji     :: RawEmoji
  }
  deriving ( Eq, Show, Generic )
  deriving ( ToJSON, FromJSON ) via CalamityJSON Reaction
  deriving ( HasID User ) via HasIDField "userID" Reaction
  deriving ( HasID Channel ) via HasIDField "channelID" Reaction
  deriving ( HasID Message ) via HasIDField "messageID" Reaction
