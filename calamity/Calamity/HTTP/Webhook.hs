-- | Webhook endpoints
module Calamity.HTTP.Webhook (
  WebhookRequest (..),
  CreateWebhookData (..),
  ModifyWebhookData (..),
  ExecuteWebhookOptions (..),
) where

import Calamity.HTTP.Channel (AllowedMentions, CreateMessageAttachment (..))
import Calamity.HTTP.Internal.Request
import Calamity.HTTP.Internal.Route
import Calamity.Internal.AesonThings
import Calamity.Types.Model.Channel
import Calamity.Types.Model.Guild
import Calamity.Types.Snowflake
import Control.Lens hiding ((.=))
import Data.Aeson
import Data.Default.Class
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics
import Network.HTTP.Client.MultipartFormData
import Network.HTTP.Req
import Network.Mime
import PyF

data CreateWebhookData = CreateWebhookData
  { username :: Maybe Text
  , -- | The avatar field should be in discord's image data format: https://discord.com/developers/docs/reference#image-data
    avatar :: Maybe Text
  }
  deriving (Show, Generic, Default)
  deriving (ToJSON) via CalamityJSON CreateWebhookData

data ModifyWebhookData = ModifyWebhookData
  { username :: Maybe Text
  , -- | The avatar field should be in discord's image data format: https://discord.com/developers/docs/reference#image-data
    avatar :: Maybe Text
  , channelID :: Maybe (Snowflake Channel)
  }
  deriving (Show, Generic, Default)
  deriving (ToJSON) via CalamityJSON ModifyWebhookData

data ExecuteWebhookOptions = ExecuteWebhookOptions
  { wait :: Maybe Bool
  , content :: Maybe Text
  , attachments :: [CreateMessageAttachment]
  , embeds :: Maybe [Embed]
  , username :: Maybe Text
  , avatarUrl :: Maybe Text
  , allowedMentions :: Maybe AllowedMentions
  , tts :: Maybe Bool
  , components :: [Component]
  }
  deriving (Show, Generic, Default)

data CreateMessageAttachmentJson = CreateMessageAttachmentJson
  { id :: Int
  , filename :: Text
  , description :: Maybe Text
  }
  deriving (Show, Generic)
  deriving (ToJSON) via CalamityJSON CreateMessageAttachmentJson

data ExecuteWebhookJson = ExecuteWebhookJson
  { content :: Maybe Text
  , embeds :: Maybe [Embed]
  , username :: Maybe Text
  , avatarUrl :: Maybe Text
  , tts :: Maybe Bool
  , attachments :: [CreateMessageAttachmentJson]
  , allowedMentions :: Maybe AllowedMentions
  , components :: [Component]
  }
  deriving (Show, Generic)
  deriving (ToJSON) via CalamityJSON ExecuteWebhookJson

data WebhookRequest a where
  CreateWebhook :: HasID Channel c => c -> CreateWebhookData -> WebhookRequest Webhook
  GetChannelWebhooks :: HasID Channel c => c -> WebhookRequest [Webhook]
  GetGuildWebhooks :: HasID Guild c => c -> WebhookRequest [Webhook]
  GetWebhook :: HasID Webhook w => w -> WebhookRequest Webhook
  GetWebhookToken :: HasID Webhook w => w -> Text -> WebhookRequest Webhook
  ModifyWebhook :: HasID Webhook w => w -> ModifyWebhookData -> WebhookRequest Webhook
  ModifyWebhookToken :: HasID Webhook w => w -> Text -> ModifyWebhookData -> WebhookRequest Webhook
  DeleteWebhook :: HasID Webhook w => w -> WebhookRequest ()
  DeleteWebhookToken :: HasID Webhook w => w -> Text -> WebhookRequest ()
  ExecuteWebhook :: HasID Webhook w => w -> Text -> ExecuteWebhookOptions -> WebhookRequest ()

baseRoute :: Snowflake Webhook -> RouteBuilder _
baseRoute id = mkRouteBuilder // S "webhooks" // ID @Webhook & giveID id

instance Request (WebhookRequest a) where
  type Result (WebhookRequest a) = a

  route (CreateWebhook (getID @Channel -> cid) _) =
    mkRouteBuilder // S "channels" // ID @Channel // S "webhooks"
      & giveID cid
      & buildRoute
  route (GetChannelWebhooks (getID @Channel -> cid)) =
    mkRouteBuilder // S "channels" // ID @Channel // S "webhooks"
      & giveID cid
      & buildRoute
  route (GetGuildWebhooks (getID @Guild -> gid)) =
    mkRouteBuilder // S "guilds" // ID @Guild // S "webhooks"
      & giveID gid
      & buildRoute
  route (GetWebhook (getID @Webhook -> wid)) =
    baseRoute wid
      & buildRoute
  route (GetWebhookToken (getID @Webhook -> wid) t) =
    baseRoute wid // S t
      & buildRoute
  route (ModifyWebhook (getID @Webhook -> wid) _) =
    baseRoute wid
      & buildRoute
  route (ModifyWebhookToken (getID @Webhook -> wid) t _) =
    baseRoute wid // S t
      & buildRoute
  route (DeleteWebhook (getID @Webhook -> wid)) =
    baseRoute wid
      & buildRoute
  route (DeleteWebhookToken (getID @Webhook -> wid) t) =
    baseRoute wid // S t
      & buildRoute
  route (ExecuteWebhook (getID @Webhook -> wid) t _) =
    baseRoute wid // S t
      & buildRoute

  action (CreateWebhook _ o) = postWith' $ ReqBodyJson o
  action (GetChannelWebhooks _) = getWith
  action (GetGuildWebhooks _) = getWith
  action (GetWebhook _) = getWith
  action (GetWebhookToken _ _) = getWith
  action (ModifyWebhook _ o) = patchWith' $ ReqBodyJson o
  action (ModifyWebhookToken _ _ o) = patchWith' $ ReqBodyJson o
  action (DeleteWebhook _) = deleteWith
  action (DeleteWebhookToken _ _) = deleteWith
  action (ExecuteWebhook _ _ wh) = \u o -> do
    let filePart CreateMessageAttachment {filename, content} n =
          (partLBS @IO [fmt|file[{n}]|] content)
            { partFilename = Just (T.unpack filename)
            , partContentType = Just (defaultMimeLookup filename)
            }
        attachmentPart CreateMessageAttachment {filename, description} n =
          CreateMessageAttachmentJson n filename description
        files = zipWith filePart (wh ^. #attachments) [(0 :: Int) ..]
        attachments = zipWith attachmentPart (wh ^. #attachments) [0 ..]
        jsonBody =
          ExecuteWebhookJson
            { content = wh ^. #content
            , username = wh ^. #username
            , avatarUrl = wh ^. #avatarUrl
            , tts = wh ^. #tts
            , embeds = wh ^. #embeds
            , allowedMentions = wh ^. #allowedMentions
            , components = wh ^. #components
            , attachments = attachments
            }
    body <- reqBodyMultipart (partLBS "payload_json" (encode jsonBody) : files)
    postWithP' body ("wait" =:? (wh ^. #wait)) u o
