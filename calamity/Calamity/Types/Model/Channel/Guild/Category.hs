{-# LANGUAGE TemplateHaskell #-}

module Calamity.Types.Model.Channel.Guild.Category (Category (..)) where

import Calamity.Internal.SnowflakeMap (SnowflakeMap)
import {-# SOURCE #-} Calamity.Types.Model.Channel
import {-# SOURCE #-} Calamity.Types.Model.Guild.Guild
import Calamity.Types.Model.Guild.Overwrite
import Calamity.Types.Snowflake
import Data.Aeson ((.!=), (.:), (.:?))
import Data.Aeson qualified as Aeson
import Data.Text (Text)
import Optics.TH
import TextShow.TH

data Category = Category
  { id :: Snowflake Category
  , permissionOverwrites :: SnowflakeMap Overwrite
  , name :: Text
  , nsfw :: Bool
  , position :: Int
  , guildID :: Snowflake Guild
  }
  deriving (Show, Eq)
  deriving (HasID Category) via HasIDField "id" Category
  deriving (HasID Channel) via HasIDFieldCoerce' "id" Category

instance Aeson.FromJSON Category where
  parseJSON = Aeson.withObject "Category" $ \v ->
    Category
      <$> v .: "id"
      <*> v .: "permission_overwrites"
      <*> v .: "name"
      <*> v .:? "nsfw" .!= False
      <*> v .: "position"
      <*> v .: "guild_id"

$(deriveTextShow ''Category)
$(makeFieldLabelsNoPrefix ''Category)
