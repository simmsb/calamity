{-# LANGUAGE TemplateHaskell #-}

-- | Guild roles
module Calamity.Types.Model.Guild.Role (Role (..)) where

import Calamity.Internal.IntColour
import Calamity.Internal.Utils
import Calamity.Types.Model.Guild.Permissions
import Calamity.Types.Snowflake
import Data.Aeson ((.:))
import qualified Data.Aeson as Aeson
import Data.Colour
import Data.Text (Text)
import Optics.TH
import qualified TextShow

data Role = Role
  { id :: Snowflake Role
  , name :: Text
  , color :: Colour Double
  , hoist :: Bool
  , position :: Int
  , permissions :: Permissions
  , managed :: Bool
  , mentionable :: Bool
  }
  deriving (Eq, Show)
  deriving (TextShow.TextShow) via TextShow.FromStringShow Role
  deriving (HasID Role) via HasIDField "id" Role
  deriving (Aeson.ToJSON) via CalamityToJSON Role

instance CalamityToJSON' Role where
  toPairs Role {..} =
    [ "id" .= id
    , "name" .= name
    , "color" .= IntColour color
    , "position" .= position
    , "permissions" .= permissions
    , "managed" .= managed
    , "mentionable" .= mentionable
    ]

instance Aeson.FromJSON Role where
  parseJSON = Aeson.withObject "Role" $ \v ->
    Role
      <$> v .: "id"
      <*> v .: "name"
      <*> (fromIntColour <$> v .: "color")
      <*> v .: "hoist"
      <*> v .: "position"
      <*> v .: "permissions"
      <*> v .: "managed"
      <*> v .: "mentionable"

$(makeFieldLabelsNoPrefix ''Role)
