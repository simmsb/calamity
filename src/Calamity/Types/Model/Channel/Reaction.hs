-- | Message reactions
module Calamity.Types.Model.Channel.Reaction
    ( Reaction(..) ) where

import           Calamity.Internal.AesonThings
import {-# SOURCE #-} Calamity.Types.Model.Channel
import {-# SOURCE #-} Calamity.Types.Model.Channel.Message
import {-# SOURCE #-} Calamity.Types.Model.Guild.Emoji
import {-# SOURCE #-} Calamity.Types.Model.Guild.Guild
import {-# SOURCE #-} Calamity.Types.Model.User
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
