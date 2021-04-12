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
  { resetTime :: UTCTime
    -- ^ The time when the bucket resets, used to heuristically wait out ratelimits
  , remaining  :: Maybe Int
    -- ^ The number of uses left in the bucket, used to heuristically wait out ratelimits
  }
  deriving ( Generic )

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
    BucketState
    B.ByteString
    -- ^ The bucket the route is in
  | ExhaustedBucket
    -- ^ We got a response but also exhausted the bucket
    LB.ByteString
    UTCTime
    -- ^ Retry after
    BucketState
    B.ByteString
    -- ^ The bucket the route is in
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
