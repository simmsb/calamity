-- | Webhook endpoints
module Calamity.HTTP.Webhook (
  WebhookRequest (..),
  CreateWebhookData (..),
  ModifyWebhookData (..),
  ExecuteWebhookOptions (..),
) where

import Calamity.HTTP.Internal.Request
import Calamity.HTTP.Internal.Route
import Calamity.Internal.AesonThings
import Calamity.Types.Model.Channel
import Calamity.Types.Model.Guild
import Calamity.Types.Snowflake

import Control.Lens hiding ((.=))

import Data.Aeson
import Data.ByteString.Lazy (ByteString)
import Data.Default.Class
import Data.Generics.Product.Subtype (upcast)
import Data.Text (Text)

import GHC.Generics

import Network.HTTP.Req

import Network.HTTP.Client.MultipartFormData
import TextShow

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
  , file :: Maybe ByteString
  , embeds :: Maybe [Embed]
  , username :: Maybe Text
  , avatarUrl :: Maybe Text
  , tts :: Maybe Bool
  }
  deriving (Show, Generic, Default)

data ExecuteWebhookJson = ExecuteWebhookJson
  { content :: Maybe Text
  , embeds :: Maybe [Embed]
  , username :: Maybe Text
  , avatarUrl :: Maybe Text
  , tts :: Maybe Bool
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
  action (ExecuteWebhook _ _ o@ExecuteWebhookOptions{file = Nothing}) =
    postWithP'
      (ReqBodyJson . upcast @ExecuteWebhookJson $ o)
      ("wait" =:? (showt <$> o ^. #wait))
  action (ExecuteWebhook _ _ wh@ExecuteWebhookOptions{file = Just f}) = \u o -> do
    body <- reqBodyMultipart [partLBS @IO "file" f, partLBS "payload_json" (encode . upcast @ExecuteWebhookJson $ wh)]
    postWithP' body ("wait" =:? (showt <$> wh ^. #wait)) u o
