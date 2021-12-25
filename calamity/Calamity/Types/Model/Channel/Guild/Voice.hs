-- | Voice channels
module Calamity.Types.Model.Channel.Guild.Voice (VoiceChannel (..)) where

import Calamity.Internal.AesonThings
import Calamity.Internal.SnowflakeMap (SnowflakeMap)
import Calamity.Internal.Utils ()
import {-# SOURCE #-} Calamity.Types.Model.Channel
import {-# SOURCE #-} Calamity.Types.Model.Channel.Guild.Category
import {-# SOURCE #-} Calamity.Types.Model.Guild.Guild
import Calamity.Types.Model.Guild.Overwrite
import Calamity.Types.Snowflake
import Control.DeepSeq (NFData)
import Data.Aeson
import Data.Text (Text)
import GHC.Generics
import TextShow
import qualified TextShow.Generic as TSG

data VoiceChannel = VoiceChannel
  { id :: Snowflake VoiceChannel
  , guildID :: Snowflake Guild
  , position :: Int
  , permissionOverwrites :: SnowflakeMap Overwrite
  , name :: Text
  , bitrate :: Int
  , userLimit :: Int
  , parentID :: Maybe (Snowflake Category)
  }
  deriving (Show, Eq, Generic, NFData)
  deriving (TextShow) via TSG.FromGeneric VoiceChannel
  deriving (ToJSON, FromJSON) via CalamityJSON VoiceChannel
  deriving (HasID VoiceChannel) via HasIDField "id" VoiceChannel
  deriving (HasID Channel) via HasIDFieldCoerce' "id" VoiceChannel
  deriving (HasID Guild) via HasIDField "guildID" VoiceChannel
