-- | Webhook endpoints
module Calamity.HTTP.Webhook
    ( WebhookRequest(..)
    , CreateWebhookData(..)
    , ModifyWebhookData(..)
    , ExecuteWebhookOptions(..) ) where

import           Calamity.HTTP.Internal.Request
import           Calamity.HTTP.Internal.Route
import           Calamity.Internal.AesonThings
import           Calamity.Types.Model.Channel
import           Calamity.Types.Model.Guild
import           Calamity.Types.Snowflake

import           Control.Lens                   hiding ( (.=) )

import           Data.Aeson
import           Data.ByteString.Lazy           ( ByteString )
import           Data.Default.Class
import           Data.Generics.Product.Subtype  ( upcast )
import           Data.Maybe
import           Data.Text                      ( Text )

import           GHC.Generics

import           Network.Wreq

import           TextShow

data CreateWebhookData = CreateWebhookData
  { username :: Maybe Text
    -- | The avatar field should be in discord's image data format: https://discordapp.com/developers/docs/reference#image-data
  , avatar   :: Maybe Text
  }
  deriving ( Show, Generic, Default )
  deriving ( ToJSON ) via CalamityJSON CreateWebhookData

data ModifyWebhookData = ModifyWebhookData
  { username  :: Maybe Text
    -- | The avatar field should be in discord's image data format: https://discordapp.com/developers/docs/reference#image-data
  , avatar    :: Maybe Text
  , channelID :: Maybe (Snowflake Channel)
  }
  deriving ( Show, Generic, Default )
  deriving ( ToJSON ) via CalamityJSON ModifyWebhookData

data ExecuteWebhookOptions = ExecuteWebhookOptions
  { wait      :: Maybe Bool
  , content   :: Maybe Text
  , file      :: Maybe ByteString
  , embeds    :: Maybe [Embed]
  , username  :: Maybe Text
  , avatarUrl :: Maybe Text
  , tts       :: Maybe Bool
  }
  deriving ( Show, Generic, Default )

data ExecuteWebhookJson = ExecuteWebhookJson
  { content   :: Maybe Text
  , embeds    :: Maybe [Embed]
  , username  :: Maybe Text
  , avatarUrl :: Maybe Text
  , tts       :: Maybe Bool
  }
  deriving ( Show, Generic )
  deriving ( ToJSON ) via CalamityJSON ExecuteWebhookJson

data WebhookRequest a where
  CreateWebhook      :: HasID Channel c => c -> CreateWebhookData ->                                   WebhookRequest Webhook
  GetChannelWebhooks :: HasID Channel c => c ->                                                        WebhookRequest [Webhook]
  GetGuildWebhooks   :: HasID Guild c => c ->                                                          WebhookRequest [Webhook]
  GetWebhook         :: HasID Webhook w => w ->                                                        WebhookRequest Webhook
  GetWebhookToken    :: HasID Webhook w => w -> Text ->                                                WebhookRequest Webhook
  ModifyWebhook      :: HasID Webhook w => w -> ModifyWebhookData ->                                   WebhookRequest Webhook
  ModifyWebhookToken :: HasID Webhook w => w -> Text -> ModifyWebhookData ->                           WebhookRequest Webhook
  DeleteWebhook      :: HasID Webhook w => w ->                                                        WebhookRequest ()
  DeleteWebhookToken :: HasID Webhook w => w -> Text ->                                                WebhookRequest ()
  ExecuteWebhook     :: HasID Webhook w => w -> Text -> ExecuteWebhookOptions -> WebhookRequest ()


baseRoute :: Snowflake Webhook -> RouteBuilder _
baseRoute id = mkRouteBuilder // S "webhooks" // ID @Webhook & giveID id

instance Request (WebhookRequest a) a where
  toRoute (CreateWebhook (getID @Channel -> cid) _) = mkRouteBuilder // S "channels" // ID @Channel // S "webhooks"
    & giveID cid
    & buildRoute
  toRoute (GetChannelWebhooks (getID @Channel -> cid)) = mkRouteBuilder // S "channels" // ID @Channel // S "webhooks"
    & giveID cid
    & buildRoute
  toRoute (GetGuildWebhooks (getID @Guild -> gid)) = mkRouteBuilder // S "guilds" // ID @Guild // S "webhooks"
    & giveID gid
    & buildRoute
  toRoute (GetWebhook (getID @Webhook -> wid)) = baseRoute wid
    & buildRoute
  toRoute (GetWebhookToken (getID @Webhook -> wid) t) = baseRoute wid // S t
    & buildRoute
  toRoute (ModifyWebhook (getID @Webhook -> wid) _) = baseRoute wid
    & buildRoute
  toRoute (ModifyWebhookToken (getID @Webhook -> wid) t _) = baseRoute wid // S t
    & buildRoute
  toRoute (DeleteWebhook (getID @Webhook -> wid)) = baseRoute wid
    & buildRoute
  toRoute (DeleteWebhookToken (getID @Webhook -> wid) t) = baseRoute wid // S t
    & buildRoute
  toRoute (ExecuteWebhook (getID @Webhook -> wid) t _) = baseRoute wid // S t
    & buildRoute

  toAction (CreateWebhook _ o) = postWith' (toJSON o)
  toAction (GetChannelWebhooks _) = getWith
  toAction (GetGuildWebhooks _) = getWith
  toAction (GetWebhook _) = getWith
  toAction (GetWebhookToken _ _) = getWith
  toAction (ModifyWebhook _ o) = patchWith' (toJSON o)
  toAction (ModifyWebhookToken _ _ o) = patchWith' (toJSON o)
  toAction (DeleteWebhook _) = deleteWith
  toAction (DeleteWebhookToken _ _) = deleteWith
  toAction (ExecuteWebhook _ _ o@ExecuteWebhookOptions { file = Nothing }) = postWithP'
    (toJSON . upcast @ExecuteWebhookJson $ o) (param "wait" .~ maybeToList (showt <$> o ^. #wait))
  toAction (ExecuteWebhook _ _ o@ExecuteWebhookOptions { file = Just f }) = postWithP'
    [partLBS @IO "file" f, partLBS "payload_json" (encode . upcast @ExecuteWebhookJson $ o)]
    (param "wait" .~ maybeToList (showt <$> o ^. #wait))
