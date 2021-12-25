-- | Updated messages
module Calamity.Types.Model.Channel.UpdatedMessage (
    UpdatedMessage (..),
) where

import Calamity.Internal.AesonThings
import Calamity.Internal.OverriddenVia
import Calamity.Internal.Utils
import Calamity.Types.Model.Channel
import Calamity.Types.Model.Channel.Component
import Calamity.Types.Model.Guild.Role
import Calamity.Types.Model.User
import Calamity.Types.Snowflake
import Data.Aeson
import Data.Text (Text)
import Data.Time
import qualified Data.Vector.Unboxing as UV
import Data.Word
import GHC.Generics
import TextShow
import qualified TextShow.Generic as TSG

data UpdatedMessage' = UpdatedMessage'
    { id :: Snowflake Message
    , channelID :: Snowflake Channel
    , content :: Maybe Text
    , editedTimestamp :: Maybe (MaybeNull (CalamityFromStringShow UTCTime))
    , tts :: Maybe Bool
    , mentionEveryone :: Maybe Bool
    , mentions :: Maybe (AesonVector (Snowflake User))
    , mentionRoles :: Maybe (AesonVector (Snowflake Role))
    , mentionChannels :: Maybe (AesonVector (Snowflake Channel))
    , attachments :: !(Maybe [Attachment])
    , embeds :: Maybe [Embed]
    , reactions :: Maybe [Reaction]
    , pinned :: Maybe Bool
    , webhookID :: Maybe (MaybeNull (Snowflake Webhook))
    , type_ :: Maybe MessageType
    , activity :: Maybe (MaybeNull (CalamityFromStringShow Object))
    , application :: Maybe (MaybeNull (CalamityFromStringShow Object))
    , messageReference :: Maybe (MaybeNull MessageReference)
    , flags :: Maybe Word64
    , stickers :: Maybe (MaybeNull [CalamityFromStringShow Object])
    , referencedMessage :: Maybe (MaybeNull Message)
    , interaction :: Maybe (MaybeNull (CalamityFromStringShow Object))
    , components :: Maybe [Component]
    }
    deriving (Generic)
    deriving (TextShow) via TSG.FromGeneric UpdatedMessage'
    deriving
        (FromJSON)
        via WithSpecialCases
                '[ "author" `ExtractFieldFrom` "id"
                 , "mentions" `ExtractArrayField` "id"
                 , "mention_channels" `ExtractArrayField` "id"
                 ]
                UpdatedMessage'

data UpdatedMessage = UpdatedMessage
    { id :: Snowflake Message
    , channelID :: Snowflake Channel
    , content :: Maybe Text
    , editedTimestamp :: Maybe (MaybeNull UTCTime)
    , tts :: Maybe Bool
    , mentionEveryone :: Maybe Bool
    , mentions :: Maybe (UV.Vector (Snowflake User))
    , mentionRoles :: Maybe (UV.Vector (Snowflake Role))
    , mentionChannels :: Maybe (UV.Vector (Snowflake Channel))
    , attachments :: !(Maybe [Attachment])
    , embeds :: Maybe [Embed]
    , reactions :: Maybe [Reaction]
    , pinned :: Maybe Bool
    , webhookID :: Maybe (MaybeNull (Snowflake Webhook))
    , type_ :: Maybe MessageType
    , activity :: Maybe (MaybeNull Object)
    , application :: Maybe (MaybeNull Object)
    , messageReference :: Maybe (MaybeNull MessageReference)
    , flags :: Maybe Word64
    , stickers :: Maybe (MaybeNull [Object])
    , referencedMessage :: Maybe (MaybeNull Message)
    , interaction :: Maybe (MaybeNull Object)
    , components :: Maybe [Component]
    }
    deriving (Show, Generic)
    deriving (TextShow, FromJSON) via OverriddenVia UpdatedMessage UpdatedMessage'
    deriving (HasID Message) via HasIDField "id" UpdatedMessage
    deriving (HasID Channel) via HasIDField "channelID" UpdatedMessage
