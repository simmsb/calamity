module Calamity.Types.Model.Channel.Guild.Category (Category (..)) where

import Calamity.Internal.AesonThings
import Calamity.Internal.SnowflakeMap (SnowflakeMap)
import Calamity.Internal.Utils ()
import {-# SOURCE #-} Calamity.Types.Model.Channel
import {-# SOURCE #-} Calamity.Types.Model.Guild.Guild
import Calamity.Types.Model.Guild.Overwrite
import Calamity.Types.Snowflake
import Control.DeepSeq (NFData)
import Data.Aeson
import Data.Text (Text)
import GHC.Generics
import TextShow
import qualified TextShow.Generic as TSG

data Category = Category
  { id :: Snowflake Category
  , permissionOverwrites :: SnowflakeMap Overwrite
  , name :: Text
  , nsfw :: Bool
  , position :: Int
  , guildID :: Snowflake Guild
  }
  deriving (Show, Eq, Generic, NFData)
  deriving (TextShow) via TSG.FromGeneric Category
  deriving (ToJSON) via CalamityJSON Category
  deriving (FromJSON) via WithSpecialCases '[IfNoneThen "nsfw" DefaultToFalse] Category
  deriving (HasID Category) via HasIDField "id" Category
  deriving (HasID Channel) via HasIDFieldCoerce' "id" Category
