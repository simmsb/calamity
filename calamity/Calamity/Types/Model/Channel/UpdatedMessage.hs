-- | Updated messages
module Calamity.Types.Model.Channel.UpdatedMessage (
    UpdatedMessage (..),
) where

import Calamity.Internal.AesonThings
import Calamity.Internal.OverriddenVia
import Calamity.Internal.Utils
import Calamity.Types.Model.Channel
import Calamity.Types.Model.Guild.Role
import Calamity.Types.Model.User
import Calamity.Types.Snowflake
import Data.Aeson
import Data.Text.Lazy (Text)
import Data.Time
import qualified Data.Vector.Unboxing as UV
import GHC.Generics
import TextShow
import qualified TextShow.Generic as TSG

data UpdatedMessage' = UpdatedMessage'
    { id :: Snowflake Message
    , channelID :: Snowflake Channel
    , content :: Maybe Text
    , editedTimestamp :: Maybe (CalamityFromStringShow UTCTime)
    , tts :: Maybe Bool
    , mentionEveryone :: Maybe Bool
    , mentions :: Maybe (AesonVector (Snowflake User))
    , mentionRoles :: Maybe (AesonVector (Snowflake Role))
    , mentionChannels :: Maybe (AesonVector (Snowflake Channel))
    , attachments :: !(Maybe [Attachment])
    , embeds :: Maybe [Embed]
    , reactions :: Maybe [Reaction]
    , pinned :: Maybe Bool
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
    , editedTimestamp :: Maybe UTCTime
    , tts :: Maybe Bool
    , mentionEveryone :: Maybe Bool
    , mentions :: Maybe (UV.Vector (Snowflake User))
    , mentionRoles :: Maybe (UV.Vector (Snowflake Role))
    , mentionChannels :: Maybe (UV.Vector (Snowflake Channel))
    , attachments :: !(Maybe [Attachment])
    , embeds :: Maybe [Embed]
    , reactions :: Maybe [Reaction]
    , pinned :: Maybe Bool
    }
    deriving (Eq, Show, Generic)
    deriving (TextShow, FromJSON) via OverriddenVia UpdatedMessage UpdatedMessage'
    deriving (HasID Message) via HasIDField "id" UpdatedMessage
    deriving (HasID Channel) via HasIDField "channelID" UpdatedMessage
