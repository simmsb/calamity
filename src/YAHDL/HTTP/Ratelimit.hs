-- | Module containing ratelimit stuff

{-# LANGUAGE MultiWayIf #-}

module YAHDL.HTTP.Ratelimit
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

import           YAHDL.HTTP.Types
import           YAHDL.HTTP.Route


newRateLimitState :: IO RateLimitState
newRateLimitState = RateLimitState <$> SC.newIO <*> E.newSet

getRateLimit :: RateLimitState -> Route -> STM Lock
getRateLimit s h = SC.focus (lookupWithDefaultM L.new) h (rateLimits s)

-- TODO: Add logging here
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

computeDiscordTimeDiff :: Integer -> UTCTime -> Int
computeDiscordTimeDiff end now = round $ diffUTCTime end' now
  where end' = end & fromInteger & posixSecondsToUTCTime

-- | Parse a ratelimit header returning the number of seconds until it resets
parseRateLimitHeader :: Response a -> Int
parseRateLimitHeader r = computeDiscordTimeDiff end now
 where
  end = r ^?! responseHeader "X-Ratelimit-Reset" . _Integer
  now = r ^?! responseHeader "Date" & parseDiscordTime & fromJust

isExhausted :: Response a -> Bool
isExhausted r = r ^?! responseHeader "X-Ratelimit-Remaining" == "0"

parseRetryAfter :: Response Value -> Int
parseRetryAfter r =
  r ^?! responseBody . key "retry_after" . _Integral `div` 1000

isGlobal :: Response Value -> Bool
isGlobal r = r ^? responseBody . key "global" . _Bool == Just True


-- Either (Either a a) b
data ShouldRetry a b
  = Retry a
  | RFail a
  | RGood b

retryRequest
  :: (MonadLog Text m)
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
        info $ "Request failed due to error response."
        doFail $ Left r
      RGood r -> pure $ Right r
    where doFail v = failAction >> pure v

-- | Return `a` instantly after unlocking `l`
unlockAndPure :: MonadIO m => Lock -> a -> m a
unlockAndPure l a = do
  liftIO . atomically $ L.release l
  pure a

-- | Return `a` instantly, after scheduling `l` to be unlocked after `d` milliseconds
scheduleUnlockAndPure :: MonadIO m => Lock -> Int -> a -> m a
scheduleUnlockAndPure l d r = do
  liftIO . void . forkIO $ do
    threadDelay $ 1000 * d
    atomically $ L.release l
  pure r

doSingleRequest
  :: (MonadIO m, MonadLog Text m)
  => Event -- ^ Global lock
  -> Lock -- ^ Local lock
  -> IO (Response LB.ByteString) -- ^ Request action
  -> m (ShouldRetry RestError Value)
doSingleRequest gl l r = do
  r' <- liftIO $ doDiscordRequest r
  case r' of
    Good v              -> unlockAndPure l $ RGood v

    ExhaustedBucket v d -> do
      info "Exhausted bucket"
      scheduleUnlockAndPure l d $ RGood v

    Ratelimited d False -> do
      info "429 ratelimited on route"
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
  :: (MonadIO m, MonadLog Text m)
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




-- TODO: routes with hashes (just steal from haskord :^^^))

-- TODO: write functions for each method
-- TODO: write types to contain parameters/ data for each
