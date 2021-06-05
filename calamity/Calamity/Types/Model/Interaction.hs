{-# OPTIONS_GHC -Wno-deferred-type-errors #-}

-- | Discord Interactions
module Calamity.Types.Model.Interaction (
    Interaction (..),
    ApplicationCommandInteractionData (..),
    ApplicationCommandInteractionDataResolved (..),
    InteractionType (..),
) where

import Calamity.Internal.AesonThings
import Calamity.Internal.OverriddenVia
import Calamity.Internal.Utils
import Calamity.Types.Model.Channel (Channel)
import Calamity.Types.Model.Channel.Component
import Calamity.Types.Model.Channel.Message (Message)
import Calamity.Types.Model.Guild (Guild, Role)
import Calamity.Types.Model.Guild.Member (Member)
import Calamity.Types.Model.User (User)
import Calamity.Types.Snowflake
import Data.Aeson
import qualified Data.HashMap.Strict as H
import Data.Scientific (toBoundedInteger)
import qualified Data.Text.Lazy as L
import GHC.Generics
import TextShow
import qualified TextShow.Generic as TSG

data Interaction = Interaction
    { id :: Snowflake Interaction
    , applicationID :: Snowflake ()
    , type_ :: InteractionType
    , data_ :: Maybe ApplicationCommandInteractionData
    , guildID :: Maybe (Snowflake Guild)
    , channelID :: Maybe (Snowflake Channel)
    , member :: Maybe Member
    , user :: Maybe User
    , token :: L.Text
    , version :: Int
    , message :: Maybe Message
    }
    deriving (Show, Generic)
    deriving (TextShow) via TSG.FromGeneric Interaction
    deriving (FromJSON) via CalamityJSON Interaction

data ApplicationCommandInteractionData = ApplicationCommandInteractionData
    { id :: Snowflake () -- no Command type yet
    , name :: L.Text
    , resolved :: Maybe ApplicationCommandInteractionDataResolved
    , -- , options :: [ApplicationCommandInteractionDataOptions]
      -- No commands yet
      customID :: L.Text
    , componentType :: ComponentType
    }
    deriving (Show, Generic)
    deriving (TextShow) via TSG.FromGeneric ApplicationCommandInteractionData
    deriving (FromJSON) via CalamityJSON ApplicationCommandInteractionData

data ApplicationCommandInteractionDataResolved' = ApplicationCommandInteractionDataResolved'
    { users :: CalamityFromStringShow (H.HashMap (Snowflake User) User)
    , members :: CalamityFromStringShow (H.HashMap (Snowflake Member) Member)
    , roles :: CalamityFromStringShow (H.HashMap (Snowflake Role) Role)
    , channels :: CalamityFromStringShow (H.HashMap (Snowflake Channel) Channel)
    }
    deriving (Generic)
    deriving (TextShow) via TSG.FromGeneric ApplicationCommandInteractionDataResolved'
    deriving (FromJSON) via CalamityJSON ApplicationCommandInteractionDataResolved'

data ApplicationCommandInteractionDataResolved = ApplicationCommandInteractionDataResolved
    { users :: H.HashMap (Snowflake User) User
    , members :: H.HashMap (Snowflake Member) Member
    , roles :: H.HashMap (Snowflake Role) Role
    , channels :: H.HashMap (Snowflake Channel) Channel
    }
    deriving (Show, Generic)
    deriving
        (TextShow, FromJSON)
        via OverriddenVia ApplicationCommandInteractionDataResolved ApplicationCommandInteractionDataResolved'

data InteractionType
    = PingType
    | ApplicationCommandType
    | MessageComponentType
    deriving (Show, Generic)
    deriving (TextShow) via TSG.FromGeneric InteractionType

-- instance ToJSON InteractionType where
--     toJSON x = toJSON @Int $ case x of
--         PingType -> 1
--         ApplicationCommandType -> 2
--         MessageComponentType -> 3

--     toEncoding x = toEncoding @Int $ case x of
--         PingType -> 1
--         ApplicationCommandType -> 2
--         MessageComponentType -> 3

instance FromJSON InteractionType where
    parseJSON = withScientific "InteractionType" $ \n -> case toBoundedInteger @Int n of
        Just 1 -> pure PingType
        Just 2 -> pure ApplicationCommandType
        Just 3 -> pure MessageComponentType
        _ -> fail $ "Invalid InteractionType: " <> show n
