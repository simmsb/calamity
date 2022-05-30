{-# LANGUAGE TemplateHaskell #-}

-- | Guild Members
module Calamity.Types.Model.Guild.Member (Member (..)) where

import Calamity.Internal.Utils (AesonVector (unAesonVector))
import Calamity.Types.Model.Guild.Role
import Calamity.Types.Model.User
import Calamity.Types.Snowflake
import Data.Aeson ((.:), (.:?), (.!=))
import qualified Data.Aeson as Aeson
import Data.Text (Text)
import Data.Time
import Data.Vector.Unboxing (Vector)
import qualified Data.Vector.Unboxing as V
import Data.Word (Word64)
import Optics.TH
import qualified TextShow

data Member = Member
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
  , nick :: Maybe Text
  , roles :: Vector (Snowflake Role)
  , joinedAt :: UTCTime
  , deaf :: Bool
  , mute :: Bool
  }
  deriving (Eq, Show)
  deriving (TextShow.TextShow) via TextShow.FromStringShow Member
  deriving (HasID Member) via HasIDFieldCoerce "id" Member User
  deriving (HasID User) via HasIDField "id" Member

instance Aeson.FromJSON Member where
  parseJSON = Aeson.withObject "Member" $ \v -> do
    u :: Aeson.Object <- v .: "user"

    Member
      <$> u .: "id"
      <*> u .: "username"
      <*> u .: "discriminator"
      <*> u .:? "bot"
      <*> u .:? "avatar"
      <*> u .:? "mfa_enabled"
      <*> u .:? "verified"
      <*> u .:? "email"
      <*> u .:? "flags"
      <*> u .:? "premium_type"
      <*> v .:? "nick"
      <*> (fmap unAesonVector <$> u .:? "roles") .!= V.empty
      <*> u .: "joined_at"
      <*> u .: "deaf"
      <*> u .: "mute"

$(makeFieldLabelsNoPrefix ''Member)
