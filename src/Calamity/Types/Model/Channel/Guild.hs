-- | The generic guild channel type
module Calamity.Types.Model.Channel.Guild
    ( GuildChannel(..)
    , module Calamity.Types.Model.Channel.Guild.Category
    , module Calamity.Types.Model.Channel.Guild.Text
    , module Calamity.Types.Model.Channel.Guild.Voice ) where

import {-# SOURCE #-} Calamity.Types.Model.Channel
import           Calamity.Types.Model.Channel.ChannelType
import           Calamity.Types.Model.Channel.Guild.Category
import           Calamity.Types.Model.Channel.Guild.Text
import           Calamity.Types.Model.Channel.Guild.Voice
import {-# SOURCE #-} Calamity.Types.Model.Guild.Guild
import           Calamity.Types.Snowflake

import           Control.Monad

import           Data.Aeson
import           Data.Generics.Product.Fields

data GuildChannel
  = GuildTextChannel TextChannel
  | GuildVoiceChannel VoiceChannel
  | GuildCategory Category
  deriving ( Show, Eq, Generic )

instance FromJSON GuildChannel where
  parseJSON = withObject "GuildChannel" $ \v -> do
    type_ <- v .: "type"

    case type_ of
      GuildTextType     -> GuildTextChannel <$> parseJSON (Object v)
      GuildVoiceType    -> GuildVoiceChannel <$> parseJSON (Object v)
      GuildCategoryType -> GuildCategory <$> parseJSON (Object v)
      _                 -> fail "Not a valid guild channel"

instance HasID GuildChannel GuildChannel where
  getID (GuildTextChannel a) = coerceSnowflake $ a ^. field' @"id"
  getID (GuildVoiceChannel a) = coerceSnowflake $ a ^. field' @"id"
  getID (GuildCategory a) = coerceSnowflake $ a ^. field' @"id"

instance HasID Channel GuildChannel where
  getID = coerceSnowflake . getID @GuildChannel

instance HasID Guild GuildChannel where
  getID (GuildTextChannel a) = coerceSnowflake $ a ^. field' @"guildID"
  getID (GuildVoiceChannel a) = coerceSnowflake $ a ^. field' @"guildID"
  getID (GuildCategory a) = coerceSnowflake $ a ^. field' @"guildID"
