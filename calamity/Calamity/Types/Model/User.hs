{-# LANGUAGE TemplateHaskell #-}

-- | A User
module Calamity.Types.Model.User (
  User (..),
  Partial (PartialUser),
  StatusType (..),
) where

import {-# SOURCE #-} Calamity.Types.Model.Guild.Member
import Calamity.Types.Partial
import Calamity.Types.Snowflake
import Data.Aeson ((.:), (.:?))
import qualified Data.Aeson as Aeson
import Data.Text (Text)
import Data.Word
import Optics.TH
import TextShow.TH

data User = User
  { id :: Snowflake User
  , username :: Text
  , discriminator :: Text
  , bot :: Maybe Bool
  , avatar :: Maybe Text
  , mfaEnabled :: Maybe Bool
  , verified :: Maybe Bool
  , email :: Maybe Text
  , flags :: Maybe Word64
  , premiumType :: Maybe Word64
  }
  deriving (Show, Eq)
  deriving (HasID User) via HasIDField "id" User
  deriving (HasID Member) via HasIDFieldCoerce' "id" User

instance Aeson.FromJSON User where
  parseJSON = Aeson.withObject "User" $ \v ->
    User
      <$> v .: "id"
      <*> v .: "username"
      <*> v .: "discriminator"
      <*> v .:? "bot"
      <*> v .:? "avatar"
      <*> v .:? "mfa_enabled"
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

$(deriveTextShow ''User)
$(deriveTextShow 'PartialUser)
$(deriveTextShow ''StatusType)
$(makeFieldLabelsNoPrefix ''User)
$(makeFieldLabelsNoPrefix 'PartialUser)
$(makeFieldLabelsNoPrefix ''StatusType)
