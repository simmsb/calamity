-- | Types for the http lib
module Calamity.HTTP.Internal.Types
    ( RestError(..)
    , RateLimitState(..)
    , DiscordResponseType(..)
    , Bucket(..)
    , BucketState(..)
    , GatewayResponse
    , BotGatewayResponse ) where

import           Calamity.HTTP.Internal.Route
import           Calamity.Internal.AesonThings

import           Control.Concurrent.Event      ( Event )
import           Control.Concurrent.STM.Lock   ( Lock )
import           Control.Concurrent.STM.TVar   ( TVar )

import Data.Time
import           Data.Aeson
import qualified Data.ByteString.Lazy          as LB
import qualified Data.ByteString          as B
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

data BucketState = BucketState
  { resetTime :: Maybe UTCTime
    -- ^ The time when the bucket resets, used to heuristically wait out ratelimits
  , resetKey  :: Int
    -- ^ The X-Ratelimit-Reset value discord gave us
  , remaining :: Int
    -- ^ The number of uses left in the bucket, used to heuristically wait out ratelimits
  , limit :: Int
    -- ^ The total number of uses for this bucket
  , ongoing :: Int
    -- ^ How many ongoing requests
  }
  deriving ( Generic, Show )

data Bucket = Bucket
  { lock :: Lock
  , state :: TVar BucketState
  }
  deriving ( Generic )

data RateLimitState = RateLimitState
  { bucketKeys :: SC.Map Route B.ByteString
  , buckets    :: SC.Map B.ByteString Bucket
  , globalLock :: Event
  }
  deriving ( Generic )

data DiscordResponseType
  = Good
    -- ^ A good response
    LB.ByteString
    (Maybe (BucketState, B.ByteString))
    -- ^ The ratelimit headers if we got them
  | Ratelimited
    -- ^ We hit a 429, no response and ratelimited
    UTCTime
    -- ^ Retry after
    Bool
    -- ^ Global ratelimit
    (Maybe (BucketState, B.ByteString))
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
