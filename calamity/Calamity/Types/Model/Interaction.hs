{-# LANGUAGE TemplateHaskell #-}

-- | Discord Interactions
module Calamity.Types.Model.Interaction (
  Interaction (..),
  InteractionToken (..),
  InteractionData (..),
  ResolvedInteractionData (..),
  InteractionType (..),
  Application,
  ApplicationCommand,
) where

import Calamity.Types.Model.Channel (Attachment, Channel, Partial)
import Calamity.Types.Model.Channel.Component
import Calamity.Types.Model.Channel.Message (Message)
import Calamity.Types.Model.Guild (Guild, Role)
import Calamity.Types.Model.Guild.Member (Member)
import Calamity.Types.Model.User (User)
import Calamity.Types.Snowflake
import Data.Aeson ((.:), (.:?))
import qualified Data.Aeson as Aeson
import qualified Data.HashMap.Strict as H
import Data.Scientific (toBoundedInteger)
import qualified Data.Text as T
import Optics.TH
import qualified TextShow
import TextShow.TH

-- | Empty type to flag application IDs
data Application

-- | Empty type to flag application command IDs
data ApplicationCommand

newtype InteractionToken = InteractionToken
  { fromInteractionToken :: T.Text
  }
  deriving stock (Show)
  deriving (Aeson.FromJSON, Aeson.ToJSON) via T.Text

data Interaction = Interaction
  { id :: Snowflake Interaction
  , applicationID :: Snowflake Application
  , type_ :: InteractionType
  , data_ :: Maybe InteractionData
  , guildID :: Maybe (Snowflake Guild)
  , channelID :: Maybe (Snowflake Channel)
  , member :: Maybe Member
  , user :: Maybe User
  , token :: InteractionToken
  , version :: Int
  , message :: Maybe Message
  , locale :: Maybe T.Text
  , guildLocale :: Maybe T.Text
  }
  deriving (Show)
  deriving (HasID Interaction) via HasIDField "id" Interaction
  deriving (HasID Application) via HasIDField "applicationID" Interaction

instance Aeson.FromJSON Interaction where
  parseJSON = Aeson.withObject "Interaction" $ \v ->
    Interaction
      <$> v .: "id"
      <*> v .: "application_id"
      <*> v .: "type"
      <*> v .:? "data"
      <*> v .:? "guild_id"
      <*> v .:? "channel_id"
      <*> v .:? "member"
      <*> v .:? "user"
      <*> v .: "token"
      <*> v .: "version"
      <*> v .:? "message"
      <*> v .:? "locale"
      <*> v .:? "guild_locale"

data InteractionData = InteractionData
  { id :: Maybe (Snowflake ApplicationCommand)
  , name :: Maybe T.Text
  , resolved :: Maybe ResolvedInteractionData
  , -- , options :: [ApplicationCommandInteractionDataOptions]
    -- No commands yet
    customID :: Maybe CustomID
  , componentType :: Maybe ComponentType
  , values :: Maybe [T.Text]
  , targetID :: Maybe (Snowflake ())
  , components :: Maybe [Aeson.Value]
  }
  deriving (Show)
  deriving (TextShow.TextShow) via TextShow.FromStringShow InteractionData

instance Aeson.FromJSON InteractionData where
  parseJSON = Aeson.withObject "InteractionData" $ \v ->
    InteractionData
      <$> v .:? "id"
      <*> v .:? "name"
      <*> v .:? "resolved"
      <*> v .:? "custom_id"
      <*> v .:? "component_type"
      <*> v .:? "values"
      <*> v .:? "target_id"
      <*> v .:? "components"

data ResolvedInteractionData = ResolvedInteractionData
  { users :: H.HashMap (Snowflake User) User
  , members :: H.HashMap (Snowflake Member) Member
  , roles :: H.HashMap (Snowflake Role) Role
  , channels :: H.HashMap (Snowflake Channel) (Partial Channel)
  , messages :: H.HashMap (Snowflake Message) (Partial Message)
  , attachments :: H.HashMap (Snowflake Attachment) Attachment
  }
  deriving (Show)
  deriving (TextShow.TextShow) via TextShow.FromStringShow ResolvedInteractionData

instance Aeson.FromJSON ResolvedInteractionData where
  parseJSON = Aeson.withObject "ResolvedInteractionData" $ \v ->
    ResolvedInteractionData
      <$> v .: "users"
      <*> v .: "members"
      <*> v .: "roles"
      <*> v .: "channels"
      <*> v .: "messages"
      <*> v .: "attachments"

data InteractionType
  = PingType
  | ApplicationCommandType
  | MessageComponentType
  | ApplicationCommandAutoCompleteType
  | ModalSubmitType
  deriving (Eq, Show)

instance Aeson.FromJSON InteractionType where
  parseJSON = Aeson.withScientific "InteractionType" $ \n -> case toBoundedInteger @Int n of
    Just 1 -> pure PingType
    Just 2 -> pure ApplicationCommandType
    Just 3 -> pure MessageComponentType
    Just 4 -> pure ApplicationCommandAutoCompleteType
    Just 5 -> pure ModalSubmitType
    _ -> fail $ "Invalid InteractionType: " <> show n

$(deriveTextShow ''InteractionToken)
$(deriveTextShow ''InteractionType)
$(deriveTextShow ''Interaction)
$(makeFieldLabelsNoPrefix ''InteractionToken)
$(makeFieldLabelsNoPrefix ''Interaction)
$(makeFieldLabelsNoPrefix ''InteractionData)
$(makeFieldLabelsNoPrefix ''ResolvedInteractionData)
$(makeFieldLabelsNoPrefix ''InteractionType)
