{-# LANGUAGE TemplateHaskell #-}

-- | Types of channels
module Calamity.Types.Model.Channel.ChannelType (ChannelType (..)) where

import Data.Aeson qualified as Aeson
import Data.Scientific
import Optics.TH
import TextShow.TH

-- Thanks sbrg (https://github.com/saevarb/haskord/blob/d1bb07bcc4f3dbc29f2dfd3351ff9f16fc100c07/haskord-lib/src/Haskord/Types/Common.hsfield#L182)
data ChannelType
  = GuildTextType
  | DMType
  | GuildVoiceType
  | GroupDMType
  | GuildCategoryType
  | GuildNewsType
  | GuildNewsThreadType
  | GuildPublicThreadType
  | GuildPrivateThreadType
  | GuildStageVoiceType
  | GuildDirectoryType
  | GuildForumType
  deriving (Eq, Show, Enum)

$(deriveTextShow ''ChannelType)
$(makeFieldLabelsNoPrefix ''ChannelType)

instance Aeson.ToJSON ChannelType where
  toJSON t = Aeson.toJSON (fromEnum t)
  toEncoding t = Aeson.toEncoding (fromEnum t)

instance Aeson.FromJSON ChannelType where
  parseJSON = Aeson.withScientific "ChannelType" $ \n -> case toBoundedInteger @Int n of
    Just v -> case v of
      0 -> pure GuildTextType
      1 -> pure DMType
      2 -> pure GuildVoiceType
      3 -> pure GroupDMType
      4 -> pure GuildCategoryType
      5 -> pure GuildNewsType
      10 -> pure GuildNewsThreadType
      11 -> pure GuildPublicThreadType
      12 -> pure GuildPrivateThreadType
      13 -> pure GuildStageVoiceType
      14 -> pure GuildDirectoryType
      15 -> pure GuildForumType
      _ -> fail $ "Invalid ChannelType: " <> show n
    Nothing -> fail $ "Invalid ChannelType: " <> show n
