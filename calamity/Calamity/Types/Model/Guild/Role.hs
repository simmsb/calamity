{-# LANGUAGE TemplateHaskell #-}

-- | Guild roles
module Calamity.Types.Model.Guild.Role (
  Role (..),
  RoleIcon (..),
) where

import Calamity.Internal.IntColour
import Calamity.Internal.Utils
import Calamity.Types.CDNAsset (CDNAsset (..))
import Calamity.Types.Model.Guild.Emoji
import Calamity.Types.Model.Guild.Permissions
import Calamity.Types.Snowflake
import Calamity.Utils.CDNUrl (assetHashFile, cdnURL)
import Data.Aeson ((.:), (.:?))
import Data.Aeson qualified as Aeson
import Data.Colour
import Data.Text (Text)
import Data.Text qualified as T
import Network.HTTP.Req ((/:), (/~))
import Optics.TH
import TextShow qualified

data RoleIcon = RoleIcon
  { roleID :: Snowflake Role
  , hash :: T.Text
  }
  deriving (Show, Eq)

instance CDNAsset RoleIcon where
  assetURL RoleIcon {hash, roleID} =
    cdnURL /: "icons" /~ roleID /: assetHashFile hash

data Role = Role
  { id :: Snowflake Role
  , name :: Text
  , color :: Colour Double
  , hoist :: Bool
  , icon :: Maybe RoleIcon
  , emoji :: Maybe RawEmoji
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
  parseJSON = Aeson.withObject "Role" $ \v -> do
    id <- v .: "id"
    icon <- (RoleIcon id <$>) <$> v .: "icon"
    emoji <- (UnicodeEmoji <$>) <$> v .:? "unicode_emoji"

    Role
      <$> pure id
      <*> v .: "name"
      <*> (fromIntColour <$> v .: "color")
      <*> v .: "hoist"
      <*> pure icon
      <*> pure emoji
      <*> v .: "position"
      <*> v .: "permissions"
      <*> v .: "managed"
      <*> v .: "mentionable"

$(makeFieldLabelsNoPrefix ''Role)
