{-# LANGUAGE TemplateHaskell #-}

-- | Guild Members
module Calamity.Types.Model.Guild.Member (Member (..)) where

import Calamity.Internal.IntColour
import Calamity.Internal.Utils (AesonVector (unAesonVector))
import Calamity.Types.Model.Avatar (Avatar (..))
import Calamity.Types.Model.Guild.Role
import Calamity.Types.Model.User
import Calamity.Types.Snowflake
import Data.Aeson ((.!=), (.:), (.:?))
import Data.Aeson qualified as Aeson
import Data.Colour (Colour)
import Data.Text (Text)
import Data.Text.Read (decimal)
import Data.Time
import Data.Vector.Unboxing (Vector)
import Data.Vector.Unboxing qualified as V
import Data.Word (Word64)
import Optics.TH
import TextShow qualified

data Member = Member
  { id :: Snowflake User
  , username :: Text
  , discriminator :: Text
  , bot :: Maybe Bool
  , avatar :: Avatar
  , memberAvatar :: Maybe Text
  , mfaEnabled :: Maybe Bool
  , banner :: Maybe UserBanner
  , accentColour :: Maybe (Colour Double)
  , locale :: Maybe Text
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
    uid <- u .: "id"
    avatarHash <- u .:? "avatar"
    discrim <- u .: "discriminator"
    discrim' <- case decimal discrim of
      Right (n, _) -> pure n
      Left e -> fail e
    let avatar = Avatar avatarHash uid discrim'
    banner <- (UserBanner uid <$>) <$> v .:? "banner"
    Member
      <$> pure uid
      <*> u .: "username"
      <*> u .: "discriminator"
      <*> v .:? "bot"
      <*> pure avatar
      <*> v .:? "avatar"
      <*> v .:? "mfa_enabled"
      <*> pure banner
      <*> (fmap fromIntColour <$> v .:? "accent_color")
      <*> v .:? "locale"
      <*> v .:? "verified"
      <*> v .:? "email"
      <*> v .:? "flags"
      <*> v .:? "premium_type"
      <*> v .:? "nick"
      <*> (fmap unAesonVector <$> v .:? "roles") .!= V.empty
      <*> v .: "joined_at"
      <*> v .: "deaf"
      <*> v .: "mute"

$(makeFieldLabelsNoPrefix ''Member)
