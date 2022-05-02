{-# LANGUAGE TemplateHaskell #-}

-- | Guild invites
module Calamity.Types.Model.Guild.Invite (Invite (..)) where

import Calamity.Types.Model.Channel
import Calamity.Types.Model.Guild.Guild
import Calamity.Types.Model.User
import Data.Aeson ((.:), (.:?))
import qualified Data.Aeson as Aeson
import Data.Text (Text)
import Data.Time (UTCTime)
import Optics.TH
import qualified TextShow

data Invite = Invite
  { code :: Text
  , guild :: Maybe (Partial Guild)
  , channel :: Maybe (Partial Channel)
  , inviter :: Maybe User
  , targetUser :: Maybe User
  , targetType :: Maybe Int
  , approximatePresenceCount :: Maybe Int
  , approximateMemberCount :: Maybe Int
  , expiresAt :: Maybe UTCTime
  }
  deriving (Eq, Show)
  deriving (TextShow.TextShow) via TextShow.FromStringShow Invite

instance Aeson.FromJSON Invite where
  parseJSON = Aeson.withObject "Invite" $ \v ->
    Invite
      <$> v .: "code"
      <*> v .:? "guild"
      <*> v .:? "channel"
      <*> v .:? "inviter"
      <*> v .:? "target_user"
      <*> v .:? "target_type"
      <*> v .:? "approximate_presence_count"
      <*> v .:? "approximate_user_count"
      <*> v .:? "expires_at"

$(makeFieldLabelsNoPrefix ''Invite)
