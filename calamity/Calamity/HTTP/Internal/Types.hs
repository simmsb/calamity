{-# LANGUAGE TemplateHaskell #-}

-- | Types for the http lib
module Calamity.HTTP.Internal.Types
    ( RestError(..)
    , RateLimitState(..)
    , DiscordResponseType(..)
    , Bucket(..)
    , BucketState(..)
    , GatewayResponse
    , BotGatewayResponse ) where

import Calamity.HTTP.Internal.Route
import           Control.Concurrent.Event      ( Event )
import           Control.Concurrent.STM.TVar   ( TVar )
import Data.Time
import           Data.Aeson
import qualified Data.ByteString.Lazy          as LB
import qualified Data.ByteString          as B
import           Data.Text as T
import qualified StmContainers.Map             as SC
import qualified Data.Aeson as Aeson
import Optics.TH

data RestError
  -- | An error response from discord
  = HTTPError
      { status   :: Int
      , response :: Maybe Value
      }
  -- | Something failed while making the request (after retrying a few times)
  | InternalClientError T.Text
  deriving ( Show)

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
  deriving ( Show )

newtype Bucket = Bucket
  { state :: TVar BucketState
  }

data RateLimitState = RateLimitState
  { bucketKeys :: SC.Map RouteKey B.ByteString
  , buckets    :: SC.Map B.ByteString Bucket
  , globalLock :: Event
  }

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
  | InternalResponseError T.Text -- ^ Something went wrong with the http client

newtype GatewayResponse = GatewayResponse
  { url :: T.Text
  }
  deriving stock ( Show )

instance Aeson.FromJSON GatewayResponse where
  parseJSON = Aeson.withObject "GatewayResponse" $ \v ->
    GatewayResponse <$> v .: "url"

data BotGatewayResponse = BotGatewayResponse
  { url    :: T.Text
  , shards :: Int
  }
  deriving ( Show )

instance Aeson.FromJSON BotGatewayResponse where
  parseJSON = Aeson.withObject "BotGatewayResponse" $ \v ->
    BotGatewayResponse
      <$> v .: "url"
      <*> v .: "shards"

$(makeFieldLabelsNoPrefix ''Bucket)
$(makeFieldLabelsNoPrefix ''BucketState)
$(makeFieldLabelsNoPrefix ''RateLimitState)
$(makeFieldLabelsNoPrefix ''GatewayResponse)
$(makeFieldLabelsNoPrefix ''BotGatewayResponse)
