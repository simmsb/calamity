-- | Text channels
module Calamity.Types.Model.Channel.Guild.Text
    ( TextChannel(..) ) where

import           Calamity.Internal.AesonThings
import {-# SOURCE #-} Calamity.Types.Model.Channel
import {-# SOURCE #-} Calamity.Types.Model.Channel.Guild.Category
import {-# SOURCE #-} Calamity.Types.Model.Channel.Message
import {-# SOURCE #-} Calamity.Types.Model.Guild.Guild
import           Calamity.Types.Model.Guild.Overwrite
import           Calamity.Types.Snowflake

import           Data.Aeson
import           Data.Time
import           Data.Vector                                 ( Vector )

data TextChannel = TextChannel
  { id                   :: Snowflake TextChannel
  , guildID              :: Snowflake Guild
  , position             :: Int
  , permissionOverwrites :: Vector Overwrite
  , name                 :: ShortText
  , topic                :: Maybe ShortText
  , nsfw                 :: Bool
  , lastMessageID        :: Maybe (Snowflake Message)
  , lastPinTimestamp     :: Maybe UTCTime
  , rateLimitPerUser     :: Maybe Int
  , parentID             :: Maybe (Snowflake Category)
  }
  deriving ( Show, Eq, Generic )
  deriving ( ToJSON ) via CalamityJSON TextChannel
  deriving ( FromJSON ) via WithSpecialCases '[IfNoneThen "nsfw" DefaultToFalse]
      TextChannel
  deriving ( HasID ) via HasIDFieldCoerce TextChannel Channel
