-- | Discord Interactions
module Calamity.Types.Model.Interaction (
  Interaction (..),
  InteractionData (..),
  ResolvedInteractionData (..),
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
import qualified Data.Text as T
import GHC.Generics
import TextShow
import qualified TextShow.Generic as TSG

data Interaction = Interaction
  { id :: Snowflake Interaction
  , applicationID :: Snowflake ()
  , type_ :: InteractionType
  , data_ :: Maybe InteractionData
  , guildID :: Maybe (Snowflake Guild)
  , channelID :: Maybe (Snowflake Channel)
  , member :: Maybe Member
  , user :: Maybe User
  , token :: T.Text
  , version :: Int
  , message :: Maybe Message
  , locale :: Maybe T.Text
  , guildLocale :: Maybe T.Text
  }
  deriving (Show, Generic)
  deriving (TextShow) via TSG.FromGeneric Interaction
  deriving (FromJSON) via CalamityJSON Interaction

data InteractionData = InteractionData
  { id :: Maybe (Snowflake ()) -- no Command type yet
  , name :: Maybe T.Text
  , resolved :: Maybe ResolvedInteractionData
  , -- , options :: [ApplicationCommandInteractionDataOptions]
    -- No commands yet
    customID :: Maybe CustomID
  , componentType :: Maybe ComponentType
  , values :: Maybe [T.Text]
  , targetID :: Maybe (Snowflake ())
  , components :: Maybe [Component]
  }
  deriving (Show, Generic)
  deriving (TextShow) via TSG.FromGeneric InteractionData
  deriving (FromJSON) via CalamityJSON InteractionData

data ResolvedInteractionData' = ResolvedInteractionData'
  { users :: CalamityFromStringShow (H.HashMap (Snowflake User) User)
  , members :: CalamityFromStringShow (H.HashMap (Snowflake Member) Member)
  , roles :: CalamityFromStringShow (H.HashMap (Snowflake Role) Role)
  , channels :: CalamityFromStringShow (H.HashMap (Snowflake Channel) Channel)
  }
  deriving (Generic)
  deriving (TextShow) via TSG.FromGeneric ResolvedInteractionData'
  deriving (FromJSON) via CalamityJSON ResolvedInteractionData'

data ResolvedInteractionData = ResolvedInteractionData
  { users :: H.HashMap (Snowflake User) User
  , members :: H.HashMap (Snowflake Member) Member
  , roles :: H.HashMap (Snowflake Role) Role
  , channels :: H.HashMap (Snowflake Channel) Channel
  }
  deriving (Show, Generic)
  deriving
    (TextShow, FromJSON)
    via OverriddenVia ResolvedInteractionData ResolvedInteractionData'

data InteractionType
  = PingType
  | ApplicationCommandType
  | MessageComponentType
  | ApplicationCommandAutoCompleteType
  | ModalSubmitType
  deriving (Show, Generic)
  deriving (TextShow) via TSG.FromGeneric InteractionType

instance FromJSON InteractionType where
  parseJSON = withScientific "InteractionType" $ \n -> case toBoundedInteger @Int n of
    Just 1 -> pure PingType
    Just 2 -> pure ApplicationCommandType
    Just 3 -> pure MessageComponentType
    Just 4 -> pure ApplicationCommandAutoCompleteType
    Just 5 -> pure ModalSubmitType
    _ -> fail $ "Invalid InteractionType: " <> show n
