-- | Types for http requests
module Calamity.HTTP.Types
    ( ChannelUpdate(..)
    , ChannelMessagesQuery(..) ) where

import           Calamity.Internal.AesonThings
import           Calamity.Types.General
import           Calamity.Types.Snowflake

import           Data.Aeson
import           Data.Default.Class

data ChannelUpdate = ChannelUpdate
  { name                 :: Maybe ShortText
  , position             :: Maybe Int
  , topic                :: Maybe ShortText
  , nsfw                 :: Maybe Bool
  , rateLimitPerUser     :: Maybe Int
  , bitrate              :: Maybe Int
  , userLimit            :: Maybe Int
  , permissionOverwrites :: Maybe [Overwrite]
  , parentID             :: Maybe (Snowflake Channel)
  }
  deriving ( Generic, Show )
  deriving anyclass ( Default )
  deriving ( ToJSON ) via CalamityJSON ChannelUpdate

data ChannelMessagesQuery
  = ChannelMessagesAround
      { around :: Snowflake Message
      }
  | ChannelMessagesBefore
      { before :: Snowflake Message
      }
  | ChannelMessagesAfter
      { after :: Snowflake Message
      }
  | ChannelMessagesLimit
      { limit :: Int
      }
  deriving ( Generic, Show )
  deriving ( ToJSON ) via CalamityJSON ChannelMessagesQuery
