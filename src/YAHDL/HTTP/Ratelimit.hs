-- | Module containing ratelimit stuff

{-# LANGUAGE MultiWayIf #-}

module YAHDL.HTTP.Ratelimit
  ( RateLimitState(..)
  , newRateLimitState
  , getRateLimit
  )
where

import qualified Control.Concurrent.Event      as E
import           Control.Concurrent.STM
import           Control.Concurrent.STM.Lock    ( Lock )
import qualified Control.Concurrent.STM.Lock   as L
import           Control.Monad
import           Control.Retry
import           Data.Aeson
import qualified Data.ByteString.Lazy          as LB
import           Data.Maybe
import           Data.Time
import           Data.Time.Clock
import           Data.Time.Clock.POSIX
import           Focus
import           Network.HTTP.Date
import           Network.HTTP.Types      hiding ( statusCode )
import           Network.Wreq
import qualified StmContainers.Map             as SC

import           YAHDL.HTTP.Route
import           YAHDL.Types.General            ( Token
                                                , formatToken
                                                )

data RateLimitState = RateLimitState
  { rateLimits :: SC.Map Route Lock
  , globalLock :: E.Event
  } deriving (Generic)

data DiscordResponseType
  -- | A good response
  = Good Value
  -- | We got a response but also exhausted the bucket
  | ExhaustedBucket Value
    Integer -- ^ Retry after (milliseconds)
  -- | We hit a 429, no response and ratelimited
  | Ratelimited
    Integer -- ^ Retry after (milliseconds)
    Bool -- ^ Global ratelimit
  -- | Discord's error, we should retry (HTTP 5XX)
  | ServerError Int
  -- | Our error, we should fail
  | ClientError Int Value

newRateLimitState :: IO RateLimitState
newRateLimitState = RateLimitState <$> SC.newIO <*> E.newSet

getRateLimit :: RateLimitState -> Route -> STM Lock
getRateLimit s h = SC.focus (lookupWithDefaultM L.new) h (rateLimits s)

doDiscordRequest :: IO (Response LB.ByteString) -> IO DiscordResponseType
doDiscordRequest r = do
  r' <- r
  let status = r' ^. responseStatus
  if
    | statusIsSuccessful status -> do
      val <- (^. responseBody) <$> asValue r'
      pure $ if isExhausted r'
        then ExhaustedBucket val $ parseRateLimitHeader r'
        else Good val
    | statusIsServerError status -> pure $ ServerError (status ^. statusCode)
    | status == status429 -> do
      rv <- asValue r'
      pure $ Ratelimited (parseRetryAfter rv) (isGlobal rv)
    | statusIsClientError status -> do
      val <- (^. responseBody) <$> asValue r'
      pure $ ClientError (status ^. statusCode) val
    | otherwise -> fail "Bogus response, discord fix your shit"

parseDiscordTime :: ByteString -> Maybe UTCTime
parseDiscordTime s = httpDateToUTC <$> parseHTTPDate s

computeDiscordTimeDiff :: Integer -> UTCTime -> Integer
computeDiscordTimeDiff end now = round $ diffUTCTime end' now
  where end' = end & fromInteger & posixSecondsToUTCTime

-- | Parse a ratelimit header returning the number of seconds until it resets
parseRateLimitHeader :: Response a -> Integer
parseRateLimitHeader r = computeDiscordTimeDiff end now
 where
  end = r ^?! responseHeader "X-Ratelimit-Reset" . _Integer
  now = r ^?! responseHeader "Date" & parseDiscordTime & fromJust

isExhausted :: Response a -> Bool
isExhausted r = r ^?! responseHeader "X-Ratelimit-Remaining" == "0"

parseRetryAfter :: Response Value -> Integer
parseRetryAfter r =
  r ^?! responseBody . key "retry_after" . _Integer `div` 1000

isGlobal :: Response Value -> Bool
isGlobal r = r ^? responseBody . key "global" . _Bool == Just True

-- TODO: routes with hashes (just steal from haskord :^^^))
-- TODO: bot state reader (token, rl states, etc)

-- TODO: write functions for each method
-- TODO: write types to contain parameters/ data for each route
