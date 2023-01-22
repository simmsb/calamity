{-# LANGUAGE TemplateHaskell #-}

-- | The generic guild channel type
module Calamity.Types.Model.Channel.Guild (
  GuildChannel (..),
  module Calamity.Types.Model.Channel.Guild.Category,
  module Calamity.Types.Model.Channel.Guild.Text,
  module Calamity.Types.Model.Channel.Guild.Voice,
) where

import {-# SOURCE #-} Calamity.Types.Model.Channel
import Calamity.Types.Model.Channel.ChannelType
import Calamity.Types.Model.Channel.Guild.Category
import Calamity.Types.Model.Channel.Guild.Text
import Calamity.Types.Model.Channel.Guild.Voice
import {-# SOURCE #-} Calamity.Types.Model.Guild.Guild
import Calamity.Types.Snowflake
import Data.Aeson ((.:))
import Data.Aeson qualified as Aeson
import Optics ((^.))
import Optics.TH
import TextShow.TH

data GuildChannel
  = GuildTextChannel TextChannel
  | GuildVoiceChannel VoiceChannel
  | GuildCategory Category
  | OtherGuildChannel (Snowflake Guild) (Snowflake GuildChannel) -- TODO
  deriving (Show, Eq)

instance Aeson.FromJSON GuildChannel where
  parseJSON = Aeson.withObject "GuildChannel" $ \v -> do
    type_ <- v .: "type"

    case type_ of
      GuildTextType -> GuildTextChannel <$> Aeson.parseJSON (Aeson.Object v)
      GuildVoiceType -> GuildVoiceChannel <$> Aeson.parseJSON (Aeson.Object v)
      GuildCategoryType -> GuildCategory <$> Aeson.parseJSON (Aeson.Object v)
      _typ -> do
        id_ <- v .: "id"
        guildID <- v .: "guild_id"
        pure $ OtherGuildChannel guildID id_

instance HasID GuildChannel GuildChannel where
  getID (GuildTextChannel a) = coerceSnowflake $ a ^. #id
  getID (GuildVoiceChannel a) = coerceSnowflake $ a ^. #id
  getID (GuildCategory a) = coerceSnowflake $ a ^. #id
  getID (OtherGuildChannel _ a) = a

instance HasID Channel GuildChannel where
  getID = coerceSnowflake . getID @GuildChannel

instance HasID Guild GuildChannel where
  getID (GuildTextChannel a) = coerceSnowflake $ a ^. #guildID
  getID (GuildVoiceChannel a) = coerceSnowflake $ a ^. #guildID
  getID (GuildCategory a) = coerceSnowflake $ a ^. #guildID
  getID (OtherGuildChannel a _) = a

$(deriveTextShow ''GuildChannel)
$(makeFieldLabelsNoPrefix ''GuildChannel)
