-- | Types for the http lib
module Calamity.HTTP.Internal.Types
    ( RestError(..)
    , RateLimitState(..)
    , DiscordResponseType(..)
    , GatewayResponse
    , BotGatewayResponse ) where

import           Calamity.HTTP.Internal.Route
import           Calamity.Internal.AesonThings

import           Control.Concurrent.Event      ( Event )
import           Control.Concurrent.STM.Lock   ( Lock )

import           Data.Aeson
import qualified Data.ByteString.Lazy          as LB
import           Data.Text.Lazy

import           GHC.Generics

import qualified StmContainers.Map             as SC

data RestError
  -- | An error response from discord
  = HTTPError
      { status   :: Int
      , response :: Maybe Value
      }
  -- | Something failed while making the request (after retrying a few times)
  | InternalClientError Text
  deriving ( Show, Generic )

data RateLimitState = RateLimitState
  { rateLimits :: SC.Map Route Lock
  , globalLock :: Event
  }
  deriving ( Generic )

data DiscordResponseType
  = Good LB.ByteString -- ^ A good response
  | ExhaustedBucket -- ^ We got a response but also exhausted the bucket
      LB.ByteString Int -- ^ Retry after (milliseconds)
  | Ratelimited -- ^ We hit a 429, no response and ratelimited
      Int -- ^ Retry after (milliseconds)
      Bool -- ^ Global ratelimit
  | ServerError Int -- ^ Discord's error, we should retry (HTTP 5XX)
  | ClientError Int LB.ByteString -- ^ Our error, we should fail
  | InternalResponseError Text -- ^ Something went wrong with the http client

newtype GatewayResponse = GatewayResponse
  { url :: Text
  }
  deriving ( Generic, Show )
  deriving ( FromJSON ) via CalamityJSON GatewayResponse

data BotGatewayResponse = BotGatewayResponse
  { url    :: Text
  , shards :: Int
  }
  deriving ( Generic, Show )
  deriving ( FromJSON ) via CalamityJSON BotGatewayResponse
