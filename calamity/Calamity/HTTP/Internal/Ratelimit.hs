-- | Module containing ratelimit stuff
module Calamity.HTTP.Internal.Ratelimit (
  newRateLimitState,
  doRequest,
) where

import Calamity.Client.Types (BotC)
import Calamity.HTTP.Internal.Route
import Calamity.HTTP.Internal.Types
import Calamity.Internal.Utils

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.Event (Event)
import qualified Control.Concurrent.Event as E
import Control.Concurrent.STM
import qualified Control.Concurrent.STM.Lock as L
import Control.Lens
import Control.Monad

import Data.Aeson
import Data.Aeson.Lens
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString          as B
import Data.Maybe
import qualified Data.Text.Lazy as LT
import Data.Time
import Data.Time.Clock.POSIX

import Fmt

import Network.HTTP.Client (responseStatus)
import Network.HTTP.Req
import Network.HTTP.Types

import Polysemy (Sem)
import qualified Polysemy as P
import qualified Polysemy.Async as P

import Prelude hiding (error)
import qualified Prelude

import qualified Control.Exception.Safe as Ex
import qualified StmContainers.Map as SC

newRateLimitState :: IO RateLimitState
newRateLimitState = RateLimitState <$> SC.newIO <*> SC.newIO <*> E.newSet

data Ratelimit
  = KnownRatelimit Bucket
  | UnknownRatelimit Route

{- performing rate limits
  1. try to fetch the lock from the ratelimiter state
  1.1. if we know the bucket then we retrieve the lock (KnownRatelimit)
  1.2. if we don't know the bucket, we just store the route (UnknownRatelimit)
  2.1. wait fot the global event (the global event is set unless we're globally ratelimited)
  2.2. wait for the local lock (locks are unlocked unless the bucket is exhausted)
  2.3. try to wait for the ratelimit to expire if it's exhausted
  3. we then perform the request, after this we'll know the bucket,
     so we update the ratelimit state with the bucket. if while performing
     the current request another request completed that made the bucket known,
     we don't try to overwrite it
  4.1. if we exceeded the local ratelimit we take the lock and release it after a delay
  4.2. if we exceeded the global ratelimit, we unset the global rl event,
       wait for it to expire, and then try again
-}

getRateLimit :: RateLimitState -> Route -> STM Ratelimit
getRateLimit s h = do
  bucketKey <- SC.lookup h $ bucketKeys s
  bucket <- join <$> sequenceA (flip SC.lookup (buckets s) <$> bucketKey)
  case bucket of
    Just bucket' ->
      pure $ KnownRatelimit bucket'
    Nothing ->
      pure $ UnknownRatelimit h

-- | Knowing the bucket for a route, and the ratelimit info, map the route to
-- the bucket key and retrieve the bucket
updateBucket :: RateLimitState -> Route -> B.ByteString -> BucketState -> STM Bucket
updateBucket s h b bucketState = do
  bucketKey <- SC.lookup h $ bucketKeys s
  case bucketKey of
    Just bucketKey' -> do
      -- if we know the bucket key here, then the bucket has already been made
      bucket <- SC.lookup bucketKey' $ buckets s
      case bucket of
        Just bucket' -> do
          modifyTVar' (bucket' ^. #state) (`mergeBucket` bucketState)
          pure bucket'
        Nothing -> Prelude.error "Not possible"
    Nothing -> do
      -- the bucket key wasn't known, make a new bucket and insert it
      lock <- L.new
      bs <- newTVar bucketState
      let bucket = Bucket lock bs
      SC.insert bucket b $ buckets s
      SC.insert b h $ bucketKeys s
      pure bucket
  where
    mergeBucket :: BucketState -> BucketState -> BucketState
    mergeBucket old new = new { remaining = new ^. #remaining <|> old ^. #remaining }

resetBucket :: Bucket -> STM ()
resetBucket bucket = modifyTVar' (bucket ^. #state) (#remaining .~ Nothing)

-- | Maybe wait for a bucket, updating its state to say we used it
useBucketOnce :: Bucket -> IO ()
useBucketOnce bucket = do
  now <- getCurrentTime
  mWaitUntil <- atomically $ do
    -- first wait on the lock
    L.wait $ bucket ^. #lock

    -- now try to estimate an expiry
    s <- readTVar $ bucket ^. #state

    let remaining = if now > s ^. #resetTime
          then Nothing
          else s ^. #remaining

    case remaining of
      Just n | n > 0 -> do
        modifyTVar (bucket ^. #state) (#remaining ?~ n - 1)
        pure Nothing
      Just _ -> do
        -- expired, lock and then reset the remaining
        L.acquire $ bucket ^. #lock
        modifyTVar (bucket ^. #state) (#remaining .~ Nothing)
        pure . Just $ s ^. #resetTime
      Nothing ->
        -- unknown, just assume we're good
        pure Nothing

  case mWaitUntil of
    Just when -> do
      threadDelayUntil when
      atomically . L.release $ bucket ^. #lock
    Nothing ->
      pure ()

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
            now <- P.embed getCurrentTime
            if isExhausted r'
              then case (parseRateLimitHeader now r', buildBucketState now r') of
                (Just !when, Just (!bs, !key)) ->
                  pure $ ExhaustedBucket resp when bs key
                _ ->
                  pure $ ServerError (statusCode status)
              else case buildBucketState now r' of
                Just (!bs, !key) ->
                  pure $ Good resp bs key
                Nothing ->
                  pure $ ServerError (statusCode status)
          | status == status429 -> do
            debug "Got 429 from discord, retrying."
            now <- P.embed getCurrentTime
            let resp = responseBody r'
            case (resp ^? _Value, buildBucketState now r') of
              (Just !rv, bs) ->
                pure $ Ratelimited (parseRetryAfter now rv) (isGlobal rv) bs
              _ ->
                pure $ ServerError (statusCode status)
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

-- | Parse a ratelimit header returning when it unlocks
parseRateLimitHeader :: HttpResponse r => UTCTime -> r -> Maybe UTCTime
parseRateLimitHeader now r = computedEnd <|> end
  where
    computedEnd :: Maybe UTCTime
    computedEnd = flip addUTCTime now <$> resetAfter

    resetAfter :: Maybe NominalDiffTime
    resetAfter = realToFrac <$> responseHeader r "X-Ratelimit-Reset-After" ^? _Just . _Double

    end :: Maybe UTCTime
    end = posixSecondsToUTCTime . realToFrac <$>
      responseHeader r "X-Ratelimit-Reset" ^? _Just . _Double

buildBucketState :: HttpResponse r => UTCTime -> r -> Maybe (BucketState, B.ByteString)
buildBucketState now r = (bs,) <$> bucketKey
  where
    remaining = responseHeader r "X-RateLimit-Remaining" ^? _Just . _Integral
    bs = BucketState now remaining
    bucketKey = responseHeader r "X-RateLimit-Bucket"

isExhausted :: HttpResponse r => r -> Bool
isExhausted r = responseHeader r "X-RateLimit-Remaining" == Just "0"

-- | Parse the retry after field, returning when to retry
parseRetryAfter :: UTCTime -> Value -> UTCTime
parseRetryAfter now r = addUTCTime retryAfter now
  where
    retryAfter = realToFrac $ r ^?! key "retry_after" . _Double

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
  Sem r (Either a b)
retryRequest maxRetries action = retryInner 0
 where
  retryInner numRetries = do
    res <- action
    case res of
      Retry r | numRetries > maxRetries -> do
        debug $ "Request failed after " +| maxRetries |+ " retries."
        pure $ Left r
      Retry _ ->
        retryInner (numRetries + 1)
      RFail r -> do
        debug "Request failed due to error response."
        pure $ Left r
      RGood r ->
        pure $ Right r

threadDelayMS :: Int -> IO ()
threadDelayMS ms = threadDelay (1000 * ms)

threadDelayUntil :: UTCTime -> IO ()
threadDelayUntil when = do
  now <- getCurrentTime
  let msUntil = round . (* 1000) . realToFrac @_ @Double $ diffUTCTime when now
  threadDelayMS msUntil

-- Run a single request
doSingleRequest ::
  BotC r =>
  RateLimitState ->
  Route ->
  -- | Global lock
  Event ->
  -- | Request action
  IO LbsResponse ->
  Sem r (ShouldRetry RestError LB.ByteString)
doSingleRequest rlstate route gl r = do
  P.embed $ E.wait (globalLock rlstate)

  rl <- P.embed . atomically $ getRateLimit rlstate route

  case rl of
    KnownRatelimit bucket ->
      P.embed $ useBucketOnce bucket

    _ -> pure ()

  r' <- doDiscordRequest r

  case r' of
    Good v bs bk -> do
      void . P.embed . atomically $ updateBucket rlstate route bk bs
      pure $ RGood v

    ExhaustedBucket v unlockWhen bs bk -> do
      debug $ "Exhausted bucket, unlocking at" +| unlockWhen |+ ""

      bucket <- P.embed . atomically $ do
        bucket <- updateBucket rlstate route bk bs
        L.acquire $ bucket ^. #lock
        pure bucket

      void . P.async $ do
        P.embed $ do
          threadDelayUntil unlockWhen
          atomically $ do
            L.release $ bucket ^. #lock
            resetBucket bucket
        debug "unlocking bucket"

      pure $ RGood v

    Ratelimited unlockWhen False (Just (bs, bk)) -> do
      debug $ "429 ratelimited on route, retrying at " +| unlockWhen |+ ""

      bucket <- P.embed . atomically $ do
        bucket <- updateBucket rlstate route bk bs
        L.acquire $ bucket ^. #lock
        pure bucket

      P.embed $ do
        threadDelayUntil unlockWhen
        atomically $ do
          L.release $ bucket ^. #lock
          resetBucket bucket

      pure $ Retry (HTTPError 429 Nothing)

    Ratelimited unlockWhen False _ -> do
      debug "Internal error (ratelimited but no headers), retrying"
      P.embed $ threadDelayUntil unlockWhen
      pure $ Retry (HTTPError 429 Nothing)

    Ratelimited unlockWhen True bs -> do
      debug "429 ratelimited globally"

      P.embed $ do
        case bs of
          Just (bs', bk) ->
            void . atomically $ updateBucket rlstate route bk bs'
          Nothing ->
            pure ()

        E.clear gl
        threadDelayUntil unlockWhen
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
doRequest rlstate route action =
  retryRequest
    5
    (doSingleRequest rlstate route (globalLock rlstate) action)
