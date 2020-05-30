-- | The generic guild channel type
module Calamity.Types.Model.Channel.Guild
    ( GuildChannel(..)
    , module Calamity.Types.Model.Channel.Guild.Category
    , module Calamity.Types.Model.Channel.Guild.Text
    , module Calamity.Types.Model.Channel.Guild.Voice ) where

import           Calamity.Internal.SnowflakeMap              ( SnowflakeMap )
import {-# SOURCE #-} Calamity.Types.Model.Channel
import           Calamity.Types.Model.Channel.ChannelType
import           Calamity.Types.Model.Channel.Guild.Category
import           Calamity.Types.Model.Channel.Guild.Text
import           Calamity.Types.Model.Channel.Guild.Voice
import {-# SOURCE #-} Calamity.Types.Model.Guild.Guild
import           Calamity.Types.Model.Guild.Overwrite
import           Calamity.Types.Snowflake

import           Control.Lens

import           Data.Aeson
import           Data.Generics.Product.Fields
import           Data.Text.Lazy                              ( Text )

import           GHC.Generics

import           TextShow
import qualified TextShow.Generic                            as TSG

data GuildChannel
  = GuildTextChannel TextChannel
  | GuildVoiceChannel VoiceChannel
  | GuildCategory Category
  deriving ( Show, Eq, Generic )
  deriving ( TextShow ) via TSG.FromGeneric GuildChannel

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

instance {-# OVERLAPS #-}HasField' "name" GuildChannel Text where
  field' = lens get set
    where
      get (GuildTextChannel (TextChannel { name })) = name
      get (GuildVoiceChannel (VoiceChannel { name })) = name
      get (GuildCategory (Category { name })) = name

      set (GuildTextChannel t) v = GuildTextChannel (t { name = v })
      set (GuildVoiceChannel t) v = GuildVoiceChannel (t { name = v })
      set (GuildCategory t) v = GuildCategory (t { name = v })

instance {-# OVERLAPS #-}HasField' "permissionOverwrites" GuildChannel (SnowflakeMap Overwrite) where
  field' = lens get set
    where
      get (GuildTextChannel (TextChannel { permissionOverwrites })) = permissionOverwrites
      get (GuildVoiceChannel (VoiceChannel { permissionOverwrites })) = permissionOverwrites
      get (GuildCategory (Category { permissionOverwrites })) = permissionOverwrites

      set (GuildTextChannel t) v = GuildTextChannel (t { permissionOverwrites = v })
      set (GuildVoiceChannel t) v = GuildVoiceChannel (t { permissionOverwrites = v })
      set (GuildCategory t) v = GuildCategory (t { permissionOverwrites = v })

instance {-# OVERLAPS #-}HasField' "position" GuildChannel Int where
  field' = lens get set
    where
      get (GuildTextChannel (TextChannel { position })) = position
      get (GuildVoiceChannel (VoiceChannel { position })) = position
      get (GuildCategory (Category { position })) = position

      set (GuildTextChannel t) v = GuildTextChannel (t { position = v })
      set (GuildVoiceChannel t) v = GuildVoiceChannel (t { position = v })
      set (GuildCategory t) v = GuildCategory (t { position = v })
