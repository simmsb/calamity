-- | Module containing ratelimit stuff
module Calamity.HTTP.Internal.Ratelimit (
  newRateLimitState,
  doRequest,
) where

import Calamity.Client.Types (BotC)
import Calamity.HTTP.Internal.Route
import Calamity.HTTP.Internal.Types
import Calamity.Internal.Utils

import Control.Concurrent
import Control.Concurrent.Event (Event)
import qualified Control.Concurrent.Event as E
import Control.Concurrent.STM
import Control.Concurrent.STM.Lock (Lock)
import qualified Control.Concurrent.STM.Lock as L
import Control.Lens
import Control.Monad

import Data.Aeson
import Data.Aeson.Lens
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LB
import Data.Functor
import Data.Maybe
import qualified Data.Text.Lazy as LT
import Data.Time
import Data.Time.Clock.POSIX

import Fmt

import Focus

import Network.HTTP.Client (responseStatus)
import Network.HTTP.Date
import Network.HTTP.Req
import Network.HTTP.Types

import Polysemy (Sem)
import qualified Polysemy as P
import qualified Polysemy.Async as P

import Prelude hiding (error)

import qualified Control.Exception.Safe as Ex
import qualified StmContainers.Map as SC

newRateLimitState :: IO RateLimitState
newRateLimitState = RateLimitState <$> SC.newIO <*> E.newSet

lookupOrInsertDefaultM :: Monad m => m a -> Focus a m a
lookupOrInsertDefaultM aM =
  casesM
    ( do
        a <- aM
        pure (a, Set a)
    )
    (\a -> pure (a, Leave))

getRateLimit :: RateLimitState -> Route -> STM Lock
getRateLimit s h = SC.focus (lookupOrInsertDefaultM L.new) h (rateLimits s)

doDiscordRequest :: BotC r => IO LbsResponse -> Sem r DiscordResponseType
doDiscordRequest r = do
  r'' <- P.embed $ Ex.catchAny (Right <$> r) (pure . Left . Ex.displayException)
  case r'' of
    Right r' -> do
      let status = responseStatus . toVanillaResponse $ r'
      if
          | statusIsSuccessful status -> do
            let resp = responseBody r'
            debug $ "Got good response from discord: " +|| status ||+ ""
            pure $
              if isExhausted r'
                then case parseRateLimitHeader r' of
                  Just !sleepTime -> ExhaustedBucket resp sleepTime
                  Nothing -> ServerError (statusCode status)
                else Good resp
          | status == status429 -> do
            debug "Got 429 from discord, retrying."
            let resp = responseBody r'
            case resp ^? _Value of
              Just rv -> pure $ Ratelimited (parseRetryAfter rv) (isGlobal rv)
              Nothing -> pure $ ClientError (statusCode status) "429 with invalid json???"
          | statusIsClientError status -> do
            let err = responseBody r'
            error $ "Something went wrong: " +|| err ||+ " response: " +|| r' ||+ ""
            pure $ ClientError (statusCode status) err
          | otherwise -> do
            debug $ "Got server error from discord: " +| statusCode status |+ ""
            pure $ ServerError (statusCode status)
    Left e -> do
      error $ "Something went wrong with the http client: " +| LT.pack e |+ ""
      pure . InternalResponseError $ LT.pack e

parseDiscordTime :: ByteString -> Maybe UTCTime
parseDiscordTime s = httpDateToUTC <$> parseHTTPDate s

computeDiscordTimeDiff :: Double -> UTCTime -> Int
computeDiscordTimeDiff end !now = round . (* 1000.0) $! diffUTCTime end' now
 where
  !end' = end & toRational & fromRational & posixSecondsToUTCTime

-- | Parse a ratelimit header returning the number of milliseconds until it resets
parseRateLimitHeader :: HttpResponse r => r -> Maybe Int
parseRateLimitHeader r = computeDiscordTimeDiff <$> end <*> now
 where
  end = responseHeader r "X-Ratelimit-Reset" ^? _Just . _Double
  now = parseDiscordTime =<< responseHeader r "Date"

isExhausted :: HttpResponse r => r -> Bool
isExhausted r = responseHeader r "X-RateLimit-Remaining" == Just "0"

parseRetryAfter :: Value -> Int
parseRetryAfter r = r ^?! key "retry_after" . _Integral

isGlobal :: Value -> Bool
isGlobal r = r ^? key "global" . _Bool == Just True

-- Either (Either a a) b
data ShouldRetry a b
  = Retry a
  | RFail a
  | RGood b

retryRequest ::
  BotC r =>
  -- | number of retries
  Int ->
  -- | action to perform
  Sem r (ShouldRetry a b) ->
  -- | action to run if max number of retries was reached
  Sem r () ->
  Sem r (Either a b)
retryRequest max_retries action failAction = retryInner 0
 where
  retryInner num_retries = do
    res <- action
    case res of
      Retry r | num_retries > max_retries -> do
        debug $ "Request failed after " +| max_retries |+ " retries."
        doFail $ Left r
      Retry _ -> retryInner (num_retries + 1)
      RFail r -> do
        debug "Request failed due to error response."
        doFail $ Left r
      RGood r -> pure $ Right r
   where
    doFail v = failAction $> v

-- Run a single request
-- NOTE: this function will only unlock the ratelimit lock if the request
-- gave a response, otherwise it will stay locked so that it can be retried again
doSingleRequest ::
  BotC r =>
  -- | Global lock
  Event ->
  -- | Local lock
  Lock ->
  -- | Request action
  IO LbsResponse ->
  Sem r (ShouldRetry RestError LB.ByteString)
doSingleRequest gl l r = do
  r' <- doDiscordRequest r
  case r' of
    Good v -> do
      P.embed . atomically $ L.release l
      pure $ RGood v
    ExhaustedBucket v d -> do
      debug $ "Exhausted bucket, unlocking after " +| d |+ "ms"
      void . P.async $ do
        P.embed $ do
          threadDelay $ 1000 * d
          atomically $ L.release l
        debug "unlocking bucket"
      pure $ RGood v
    Ratelimited d False -> do
      debug $ "429 ratelimited on route, sleeping for " +| d |+ " ms"
      P.embed . threadDelay $ 1000 * d
      pure $ Retry (HTTPError 429 Nothing)
    Ratelimited d True -> do
      debug "429 ratelimited globally"
      P.embed $ do
        E.clear gl
        threadDelay $ 1000 * d
        E.set gl
      pure $ Retry (HTTPError 429 Nothing)
    ServerError c -> do
      debug "Server failed, retrying"
      pure $ Retry (HTTPError c Nothing)
    InternalResponseError c -> do
      debug "Internal error, retrying"
      pure $ Retry (InternalClientError c)
    ClientError c v -> pure $ RFail (HTTPError c $ decode v)

doRequest :: BotC r => RateLimitState -> Route -> IO LbsResponse -> Sem r (Either RestError LB.ByteString)
doRequest rlState route action = do
  P.embed $ E.wait (globalLock rlState)

  ratelimit <- P.embed . atomically $ do
    lock <- getRateLimit rlState route
    L.acquire lock
    pure lock

  retryRequest
    5
    (doSingleRequest (globalLock rlState) ratelimit action)
    (P.embed . atomically $ L.release ratelimit)
