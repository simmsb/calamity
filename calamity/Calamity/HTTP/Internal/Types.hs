{-# LANGUAGE TemplateHaskell #-}

-- | Types for the http lib
module Calamity.HTTP.Internal.Types (
  RestError (..),
  RateLimitState (..),
  DiscordResponseType (..),
  Bucket (..),
  BucketState (..),
  GatewayResponse,
  BotGatewayResponse,
) where

import Calamity.HTTP.Internal.Route
import Control.Concurrent.Event (Event)
import Control.Concurrent.STM.TVar (TVar)
import Data.Aeson
import Data.Aeson qualified as Aeson
import Data.ByteString qualified as B
import Data.ByteString.Lazy qualified as LB
import Data.Text as T
import Data.Time
import Optics.TH
import StmContainers.Map qualified as SC

data RestError
  = -- | An error response from discord
    HTTPError
      { status :: Int
      , response :: Maybe Value
      }
  | -- | Something failed while making the request (after retrying a few times)
    InternalClientError T.Text
  deriving (Show)

data BucketState = BucketState
  { resetTime :: Maybe UTCTime
  -- ^ The time when the bucket resets, used to heuristically wait out ratelimits
  , resetKey :: Int
  -- ^ The X-Ratelimit-Reset value discord gave us
  , remaining :: Int
  -- ^ The number of uses left in the bucket, used to heuristically wait out ratelimits
  , limit :: Int
  -- ^ The total number of uses for this bucket
  , ongoing :: Int
  -- ^ How many ongoing requests
  }
  deriving (Show)

newtype Bucket = Bucket
  { state :: TVar BucketState
  }

data RateLimitState = RateLimitState
  { bucketKeys :: SC.Map RouteKey B.ByteString
  , buckets :: SC.Map B.ByteString Bucket
  , globalLock :: Event
  }

data DiscordResponseType
  = -- | A good response
    Good
      LB.ByteString
      -- | The ratelimit headers if we got them
      (Maybe (BucketState, B.ByteString))
  | -- | We hit a 429, no response and ratelimited
    Ratelimited
      -- | Retry after
      UTCTime
      -- | Global ratelimit
      Bool
      (Maybe (BucketState, B.ByteString))
  | -- | Discord's error, we should retry (HTTP 5XX)
    ServerError Int
  | -- | Our error, we should fail
    ClientError Int LB.ByteString
  | -- | Something went wrong with the http client
    InternalResponseError T.Text

newtype GatewayResponse = GatewayResponse
  { url :: T.Text
  }
  deriving stock (Show)

instance Aeson.FromJSON GatewayResponse where
  parseJSON = Aeson.withObject "GatewayResponse" $ \v ->
    GatewayResponse <$> v .: "url"

data BotGatewayResponse = BotGatewayResponse
  { url :: T.Text
  , shards :: Int
  }
  deriving (Show)

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
