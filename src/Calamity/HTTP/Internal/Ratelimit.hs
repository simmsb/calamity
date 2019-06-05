-- | Module containing ratelimit stuff

{-# LANGUAGE MultiWayIf #-}

module Calamity.HTTP.Internal.Ratelimit
  ( RateLimitState(..)
  , newRateLimitState
  , doRequest
  )
where

import           Control.Concurrent.Event       ( Event )
import qualified Control.Concurrent.Event      as E
import           Control.Concurrent.STM.Lock    ( Lock )
import qualified Control.Concurrent.STM.Lock   as L
import           Control.Monad
import           Data.Aeson
import qualified Data.ByteString.Lazy          as LB
import           Data.Maybe
import           Data.Time
import           Data.Time.Clock.POSIX
import           Focus
import           Network.HTTP.Date
import           Network.HTTP.Types      hiding ( statusCode )
import           Network.Wreq
import qualified StmContainers.Map             as SC

import           Calamity.HTTP.Internal.Types
import           Calamity.HTTP.Internal.Route


newRateLimitState :: IO RateLimitState
newRateLimitState = RateLimitState <$> SC.newIO <*> E.newSet

lookupOrInsertDefaultM :: Monad m => m a -> Focus a m a
lookupOrInsertDefaultM aM = casesM
  (do a <- aM
      pure (a, Set a))
  (\a -> pure (a, Leave))

getRateLimit :: RateLimitState -> Route -> STM Lock
getRateLimit s h = SC.focus (lookupOrInsertDefaultM L.new) h (rateLimits s)

-- TODO: Add logging here
doDiscordRequest
  :: (MonadIO m, MonadLog m)
  => IO (Response LB.ByteString)
  -> m DiscordResponseType
doDiscordRequest r = do
  r' <- liftIO r
  let status = r' ^. responseStatus
  if
    | statusIsSuccessful status -> do
      val <- liftIO $ (^. responseBody) <$> asValue r'
      debug $ "Got good response from discord: " +|| r' ||+ ""
      pure $ if isExhausted r'
        then ExhaustedBucket val $ parseRateLimitHeader r'
        else Good val
    | statusIsServerError status -> do
      info $ "Got server error from discord: " +| status ^. statusCode |+ ""
      pure $ ServerError (status ^. statusCode)
    | status == status429 -> do
      info "Got 429 from discord, retrying."
      rv <- liftIO $ asValue r'
      pure $ Ratelimited (parseRetryAfter rv) (isGlobal rv)
    | statusIsClientError status -> do
      val <- liftIO $ (^. responseBody) <$> asValue r'
      error $ "You fucked up: " +|| val ||+ " response: " +|| r' ||+ ""
      pure $ ClientError (status ^. statusCode) val
    | otherwise -> fail "Bogus response, discord fix your shit"

parseDiscordTime :: ByteString -> Maybe UTCTime
parseDiscordTime s = httpDateToUTC <$> parseHTTPDate s

computeDiscordTimeDiff :: Integer -> UTCTime -> Int
computeDiscordTimeDiff end now = (* 1000) . round $ diffUTCTime end' now
  where end' = end & fromInteger & posixSecondsToUTCTime

-- | Parse a ratelimit header returning the number of milliseconds until it resets
parseRateLimitHeader :: Response a -> Int
parseRateLimitHeader r = computeDiscordTimeDiff end now
 where
  end = r ^?! responseHeader "X-Ratelimit-Reset" . _Integer
  now = r ^?! responseHeader "Date" & parseDiscordTime & fromJust

isExhausted :: Response a -> Bool
isExhausted r = r ^? responseHeader "X-RateLimit-Remaining" == Just "0"

parseRetryAfter :: Response Value -> Int
parseRetryAfter r =
  r ^?! responseBody . key "retry_after" . _Integral

isGlobal :: Response Value -> Bool
isGlobal r = r ^? responseBody . key "global" . _Bool == Just True


-- Either (Either a a) b
data ShouldRetry a b
  = Retry a
  | RFail a
  | RGood b

retryRequest
  :: (MonadLog m)
  => Int -- ^ number of retries
  -> m (ShouldRetry a b) -- ^ action to perform
  -> m ()  -- ^ action to run if max number of retries was reached
  -> m (Either a b)
retryRequest max_retries action failAction = retryInner 0
 where
  retryInner num_retries = do
    res <- action
    case res of
      Retry r | num_retries > max_retries -> do
        info $ "Request failed after " +| max_retries |+ " retries."
        doFail $ Left r
      Retry _ -> retryInner (succ num_retries)
      RFail r -> do
        info "Request failed due to error response."
        doFail $ Left r
      RGood r -> pure $ Right r
    where doFail v = failAction $> v


-- Run a single request
-- NOTE: this function will only unlock the ratelimit lock if the request
-- gave a response, otherwise it will stay locked so that it can be retried again
doSingleRequest
  :: (MonadIO m, MonadLog m)
  => Event -- ^ Global lock
  -> Lock -- ^ Local lock
  -> IO (Response LB.ByteString) -- ^ Request action
  -> m (ShouldRetry RestError Value)
doSingleRequest gl l r = do
  r' <- doDiscordRequest r
  case r' of
    Good v -> do
      liftIO . atomically $ L.release l
      pure $ RGood v

    ExhaustedBucket v d -> do
      info $ "Exhausted bucket, unlocking after " +| d |+ "ms"
      void . liftIO . forkIO $ do
        threadDelay $ 1000 * d
        atomically $ L.release l
      pure $ RGood v

    Ratelimited d False -> do
      info $ "429 ratelimited on route, sleeping for " +| d |+ " ms"
      liftIO . threadDelay $ 1000 * d
      pure $ Retry (HTTPError 429 Nothing)

    Ratelimited d True -> do
      info "429 ratelimited globally"
      liftIO $ do
        E.clear gl
        threadDelay $ 1000 * d
        E.set gl
      pure $ Retry (HTTPError 429 Nothing)

    ServerError c -> do
      info "Server failed, retrying"
      pure $ Retry (HTTPError c Nothing)

    ClientError c v -> pure $ RFail (HTTPError c (Just v))

doRequest
  :: (MonadIO m, MonadLog m)
  => RateLimitState
  -> Route
  -> IO (Response LB.ByteString)
  -> m (Either RestError Value)
doRequest rlState route action = do
  liftIO $ E.wait (globalLock rlState)

  ratelimit <- liftIO . atomically $ do
    lock <- getRateLimit rlState route
    L.acquire lock
    pure lock

  retryRequest 5
               (doSingleRequest (globalLock rlState) ratelimit action)
               (liftIO . atomically $ L.release ratelimit)
