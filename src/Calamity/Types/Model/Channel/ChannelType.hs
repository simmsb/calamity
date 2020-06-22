-- | Types of channels
module Calamity.Types.Model.Channel.ChannelType
    ( ChannelType(..) ) where

import           Data.Aeson
import           Data.Scientific

import           GHC.Generics

import           TextShow
import qualified TextShow.Generic as TSG

-- Thanks sbrg (https://github.com/saevarb/haskord/blob/d1bb07bcc4f3dbc29f2dfd3351ff9f16fc100c07/haskord-lib/src/Haskord/Types/Common.hsfield#L182)
data ChannelType
  = GuildTextType
  | DMType
  | GuildVoiceType
  | GroupDMType
  | GuildCategoryType
  deriving ( Eq, Generic, Show, Enum )
  deriving ( TextShow ) via TSG.FromGeneric ChannelType

instance ToJSON ChannelType where
  toJSON t = Number $ fromIntegral (fromEnum t)

instance FromJSON ChannelType where
  parseJSON = withScientific "ChannelType" $ \n -> case toBoundedInteger @Int n of
    Just v  -> case v of
      0 -> pure GuildTextType
      1 -> pure DMType
      2 -> pure GuildVoiceType
      4 -> pure GuildCategoryType
      _ -> fail $ "Invalid ChannelType: " <> show n
    Nothing -> fail $ "Invalid ChannelType: " <> show n
