{-# LANGUAGE TemplateHaskell #-}

-- | The generic guild channel type
module Calamity.Types.Model.Channel.Guild (
  GuildChannel (..),
  module Calamity.Types.Model.Channel.Guild.Category,
  module Calamity.Types.Model.Channel.Guild.Text,
  module Calamity.Types.Model.Channel.Guild.Voice,
) where

import Calamity.Internal.SnowflakeMap (SnowflakeMap)
import {-# SOURCE #-} Calamity.Types.Model.Channel
import Calamity.Types.Model.Channel.ChannelType
import Calamity.Types.Model.Channel.Guild.Category
import Calamity.Types.Model.Channel.Guild.Text
import Calamity.Types.Model.Channel.Guild.Voice
import {-# SOURCE #-} Calamity.Types.Model.Guild.Guild
import Calamity.Types.Model.Guild.Overwrite
import Calamity.Types.Snowflake
import Data.Aeson ((.:))
import qualified Data.Aeson as Aeson
import Data.Text (Text)
import Optics ((&), (.~), (^.))
import qualified Optics
import Optics.TH
import TextShow.TH

data GuildChannel
  = GuildTextChannel TextChannel
  | GuildVoiceChannel VoiceChannel
  | GuildCategory Category
  deriving (Show, Eq)

instance Aeson.FromJSON GuildChannel where
  parseJSON = Aeson.withObject "GuildChannel" $ \v -> do
    type_ <- v .: "type"

    case type_ of
      GuildTextType -> GuildTextChannel <$> Aeson.parseJSON (Aeson.Object v)
      GuildVoiceType -> GuildVoiceChannel <$> Aeson.parseJSON (Aeson.Object v)
      GuildCategoryType -> GuildCategory <$> Aeson.parseJSON (Aeson.Object v)
      typ -> fail $ "Not a valid guild channel: " <> show typ

instance HasID GuildChannel GuildChannel where
  getID (GuildTextChannel a) = coerceSnowflake $ a ^. #id
  getID (GuildVoiceChannel a) = coerceSnowflake $ a ^. #id
  getID (GuildCategory a) = coerceSnowflake $ a ^. #id

instance HasID Channel GuildChannel where
  getID = coerceSnowflake . getID @GuildChannel

instance HasID Guild GuildChannel where
  getID (GuildTextChannel a) = coerceSnowflake $ a ^. #guildID
  getID (GuildVoiceChannel a) = coerceSnowflake $ a ^. #guildID
  getID (GuildCategory a) = coerceSnowflake $ a ^. #guildID

instance (k ~ Optics.A_Lens) => Optics.LabelOptic "name" k GuildChannel GuildChannel Text Text where
  labelOptic = Optics.lensVL $ \f s -> case s of
    GuildTextChannel c ->
      fmap
        (\y -> GuildTextChannel (c & Optics.labelOptic @"name" .~ y))
        (f $ c ^. Optics.labelOptic @"name")
    GuildVoiceChannel c ->
      fmap
        (\y -> GuildVoiceChannel (c & Optics.labelOptic @"name" .~ y))
        (f $ c ^. Optics.labelOptic @"name")
    GuildCategory c ->
      fmap
        (\y -> GuildCategory (c & Optics.labelOptic @"name" .~ y))
        (f $ c ^. Optics.labelOptic @"name")

instance (k ~ Optics.A_Lens) => Optics.LabelOptic "permissionOverwrites" k GuildChannel GuildChannel (SnowflakeMap Overwrite) (SnowflakeMap Overwrite) where
  labelOptic = Optics.lensVL $ \f s -> case s of
    GuildTextChannel c ->
      fmap
        (\y -> GuildTextChannel (c & Optics.labelOptic @"permissionOverwrites" .~ y))
        (f $ c ^. Optics.labelOptic @"permissionOverwrites")
    GuildVoiceChannel c ->
      fmap
        (\y -> GuildVoiceChannel (c & Optics.labelOptic @"permissionOverwrites" .~ y))
        (f $ c ^. Optics.labelOptic @"permissionOverwrites")
    GuildCategory c ->
      fmap
        (\y -> GuildCategory (c & Optics.labelOptic @"permissionOverwrites" .~ y))
        (f $ c ^. Optics.labelOptic @"permissionOverwrites")

$(deriveTextShow ''GuildChannel)
$(makeFieldLabelsNoPrefix ''GuildChannel)
