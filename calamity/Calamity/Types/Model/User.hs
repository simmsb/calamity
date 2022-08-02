{-# LANGUAGE TemplateHaskell #-}

-- | A User
module Calamity.Types.Model.User (
  User (..),
  UserBanner (..),
  Partial (PartialUser),
  StatusType (..),
) where

import Calamity.Internal.IntColour
import Calamity.Types.CDNAsset (CDNAsset (..))
import Calamity.Types.Model.Avatar
import {-# SOURCE #-} Calamity.Types.Model.Guild.Member
import Calamity.Types.Partial
import Calamity.Types.Snowflake
import Calamity.Utils.CDNUrl (assetHashFile, cdnURL)
import Data.Aeson ((.:), (.:?))
import qualified Data.Aeson as Aeson
import Data.Colour (Colour)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Read (decimal)
import Data.Word
import Network.HTTP.Req ((/:), (/~))
import Optics.TH
import qualified TextShow
import TextShow.TH

data UserBanner = UserBanner
  { userID :: Snowflake User
  , hash :: T.Text
  }
  deriving (Show, Eq)

instance CDNAsset UserBanner where
  assetURL UserBanner {hash, userID} =
    cdnURL /: "banners" /~ userID /: assetHashFile hash

data User = User
  { id :: Snowflake User
  , username :: Text
  , discriminator :: Text
  , bot :: Maybe Bool
  , avatar :: Avatar
  , mfaEnabled :: Maybe Bool
  , banner :: Maybe UserBanner
  , accentColour :: Maybe (Colour Double)
  , locale :: Maybe Text
  , verified :: Maybe Bool
  , email :: Maybe Text
  , flags :: Maybe Word64
  , premiumType :: Maybe Word64
  }
  deriving (Show, Eq)
  deriving (TextShow.TextShow) via TextShow.FromStringShow User
  deriving (HasID User) via HasIDField "id" User
  deriving (HasID Member) via HasIDFieldCoerce' "id" User

instance Aeson.FromJSON User where
  parseJSON = Aeson.withObject "User" $ \v -> do
    uid <- v .: "id"
    avatarHash <- v .:? "avatar"
    discrim <- v .: "discriminator"
    discrim' <- case decimal discrim of
      Right (n, _) -> pure n
      Left e -> fail e
    let avatar = Avatar avatarHash uid discrim'
    banner <- (UserBanner uid <$>) <$> v .:? "banner"
    User
      <$> pure uid
      <*> v .: "username"
      <*> v .: "discriminator"
      <*> v .:? "bot"
      <*> pure avatar
      <*> v .:? "mfa_enabled"
      <*> pure banner
      <*> (fmap fromIntColour <$> v .:? "accent_color")
      <*> v .:? "locale"
      <*> v .:? "verified"
      <*> v .:? "email"
      <*> v .:? "flags"
      <*> v .:? "premium_type"

newtype instance Partial User = PartialUser
  { id :: Snowflake User
  }
  deriving (Show, Eq)
  deriving (HasID User) via HasIDField "id" (Partial User)

instance Aeson.FromJSON (Partial User) where
  parseJSON = Aeson.withObject "Partial User" $ \v ->
    PartialUser <$> v .: "id"

data StatusType
  = Idle
  | DND
  | Online
  | Offline
  | Invisible
  deriving (Eq, Show, Enum)

instance Aeson.FromJSON StatusType where
  parseJSON = Aeson.withText "StatusType" $ \case
    "idle" -> pure Idle
    "dnd" -> pure DND
    "online" -> pure Online
    "offline" -> pure Offline
    "invisible" -> pure Invisible
    _ -> fail "Unknown status type"

instance Aeson.ToJSON StatusType where
  toJSON =
    Aeson.toJSON @Text . \case
      Idle -> "idle"
      DND -> "dnd"
      Online -> "online"
      Offline -> "offline"
      Invisible -> "invisible"

$(deriveTextShow 'PartialUser)
$(deriveTextShow ''StatusType)
$(makeFieldLabelsNoPrefix ''User)
$(makeFieldLabelsNoPrefix 'PartialUser)
$(makeFieldLabelsNoPrefix ''StatusType)
