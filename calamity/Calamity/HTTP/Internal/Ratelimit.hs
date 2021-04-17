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
import Control.Lens
import Control.Monad

import Data.Aeson
import Data.Aeson.Lens
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
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

import Prelude hiding (error)
import qualified Prelude

import qualified Control.Exception.Safe as Ex
import qualified StmContainers.Map as SC

newRateLimitState :: IO RateLimitState
newRateLimitState = RateLimitState <$> SC.newIO <*> SC.newIO <*> E.newSet

data Ratelimit
  = KnownRatelimit Bucket
  | UnknownRatelimit Route

getRateLimit :: RateLimitState -> Route -> STM Ratelimit
getRateLimit s h = do
  bucketKey <- SC.lookup h $ bucketKeys s
  bucket <- join <$> sequenceA (flip SC.lookup (buckets s) <$> bucketKey)
  case bucket of
    Just bucket' ->
      pure $ KnownRatelimit bucket'
    Nothing ->
      pure $ UnknownRatelimit h

{- | Knowing the bucket for a route, and the ratelimit info, map the route to
 the bucket key and retrieve the bucket
-}
updateBucket :: RateLimitState -> Route -> B.ByteString -> BucketState -> STM Bucket
updateBucket s h b bucketState = do
  bucketKey <- SC.lookup h $ bucketKeys s
  case bucketKey of
    Just bucketKey' -> do
      -- if we know the bucket key here, then the bucket has already been made
      -- if the given bucket key is different than the known bucket key then oops
      bucket <- SC.lookup bucketKey' $ buckets s
      case bucket of
        Just bucket' -> do
          modifyTVar' (bucket' ^. #state) (`mergeStates` bucketState)
          pure bucket'
        Nothing -> Prelude.error "Not possible"
    Nothing -> do
      -- the bucket key wasn't known, make a new bucket and insert it
      bs <- Bucket <$> newTVar bucketState
      SC.insert bs b $ buckets s
      SC.insert b h $ bucketKeys s
      pure bs
 where
  mergeStates :: BucketState -> BucketState -> BucketState
  mergeStates old new =
    new
      { ongoing = old ^. #ongoing
      , -- we only ignore the previous 'remaining' if we've not reset yet and the
        -- reset time has changed
        remaining =
          if (isJust $ old ^. #resetTime) && (old ^. #resetKey /= new ^. #resetKey)
            then min (old ^. #remaining) (new ^. #remaining)
            else new ^. #remaining
      , -- only take the new resetTime if it actually changed
        resetTime =
          if old ^. #resetKey /= new ^. #resetKey
            then new ^. #resetTime
            else old ^. #resetTime
      }

resetBucket :: Bucket -> STM ()
resetBucket bucket =
  modifyTVar'
    (bucket ^. #state)
    ( \bs ->
        bs & #remaining .~ bs ^. #limit
          & #resetTime .~ Nothing
    )

canResetBucketNow :: UTCTime -> BucketState -> Bool
canResetBucketNow _ BucketState{ongoing} | ongoing > 0 = False
-- don't allow resetting the bucket if there's ongoing requests, we'll wait
-- until another request finishes and updates the counter
canResetBucketNow now bs = case bs ^. #resetTime of
  Just rt -> now > rt
  Nothing -> False

-- canResetBucket :: BucketState -> Bool
-- canResetBucket bs = isNothing $ bs ^. #startedWaitingTime

shouldWaitForUnlock :: BucketState -> Bool
shouldWaitForUnlock BucketState{remaining = 0, ongoing} = ongoing > 0
shouldWaitForUnlock _ = False

data WaitDelay
  = WaitUntil UTCTime
  | WaitRetrySoon
  | GoNow
  deriving (Show)

intoWaitDelay :: Maybe UTCTime -> WaitDelay
intoWaitDelay (Just t) = WaitUntil t
intoWaitDelay Nothing = WaitRetrySoon

-- | Maybe wait for a bucket, updating its state to say we used it
useBucketOnce :: Bucket -> IO ()
useBucketOnce bucket = go 0
 where
  go :: Int -> IO ()
  go tries = do
    now <- getCurrentTime
    mWaitDelay <- atomically $ do
      s <- readTVar $ bucket ^. #state

      -- -- [0]
      -- -- if there are ongoing requests, wait for them to finish and deliver
      -- -- truth on the current ratelimit state
      when
        (shouldWaitForUnlock s)
        retry

      -- if there are no ongoing requests, and the bucket reset time has lapsed,
      -- we can just reset the bucket.
      --
      -- if we've already reset the bucket then there should be an ongoing
      -- request so we'll just end up waiting for that to finish
      when
        (canResetBucketNow now s)
        (resetBucket bucket)

      s <- readTVar $ bucket ^. #state

      if s ^. #remaining - s ^. #ongoing > 0
        then do
          -- there are tokens remaining for us to use
          modifyTVar'
            (bucket ^. #state)
            ( (#remaining -~ 1)
                . (#ongoing +~ 1)
            )
          pure GoNow
        else do
          -- the bucket has expired, there are no ongoing requests because of
          -- [0] wait and then retry after we can unlock the bucket
          pure (intoWaitDelay $ s ^. #resetTime)

    -- putStrLn (show now <> ": Using bucket, waiting until: " <> show mWaitDelay <> ", uses: " <> show s <> ", " <> inf)

    case mWaitDelay of
      WaitUntil waitUntil -> do
        if waitUntil < now
          then threadDelayMS 20
          else -- if the reset is in the past, we're fucked
            threadDelayUntil waitUntil
        -- if we needed to sleep, go again so that multiple concurrent requests
        -- don't exceed the bucket, to ensure we don't sit in a loop if a
        -- request dies on us, bail out after 50 loops
        if tries < 50
          then go (tries + 1)
          else pure () -- print "bailing after number of retries"
      WaitRetrySoon -> do
        threadDelayMS 20
        if tries < 50
          then go (tries + 1)
          else pure () -- print "bailing after number of retries"
      GoNow -> do
        -- print "ok going forward with request"
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
            let rlHeaders = buildBucketState now r'
            pure $ Good resp rlHeaders
          | status == status429 -> do
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
  end =
    posixSecondsToUTCTime . realToFrac
      <$> responseHeader r "X-Ratelimit-Reset" ^? _Just . _Double

buildBucketState :: HttpResponse r => UTCTime -> r -> Maybe (BucketState, B.ByteString)
buildBucketState now r = (,) <$> bs <*> bucketKey
 where
  remaining = responseHeader r "X-RateLimit-Remaining" ^? _Just . _Integral
  limit = responseHeader r "X-RateLimit-Limit" ^? _Just . _Integral
  resetKey = ceiling <$> responseHeader r "X-RateLimit-Reset" ^? _Just . _Double
  resetTime = parseRateLimitHeader now r
  bs = BucketState resetTime <$> resetKey <*> remaining <*> limit <*> pure 0
  bucketKey = responseHeader r "X-RateLimit-Bucket"

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

tenMS :: NominalDiffTime
tenMS = 0.01

threadDelayUntil :: UTCTime -> IO ()
threadDelayUntil when = do
  let when' = addUTCTime tenMS when -- lol
  now <- getCurrentTime
  let msUntil = ceiling . (* 1000) . realToFrac @_ @Double $ diffUTCTime when' now
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
    _ -> debug "unknown ratelimit"

  r' <- doDiscordRequest r

  case r' of
    Good v rlHeaders -> do
      void . P.embed . atomically $ do
        case rl of
          KnownRatelimit bucket ->
            modifyTVar' (bucket ^. #state) (#ongoing -~ 1)
          _ -> pure ()
        case rlHeaders of
          Just (bs, bk) ->
            void $ updateBucket rlstate route bk bs
          Nothing -> pure ()
      pure $ RGood v
    Ratelimited unlockWhen False (Just (bs, bk)) -> do
      debug $ "429 ratelimited on route, retrying at " +| unlockWhen |+ ""

      P.embed . atomically $ do
        case rl of
          KnownRatelimit bucket ->
            modifyTVar' (bucket ^. #state) (#ongoing -~ 1)
          _ -> pure ()
        void $ updateBucket rlstate route bk bs

      P.embed $ do
        threadDelayUntil unlockWhen

      pure $ Retry (HTTPError 429 Nothing)
    Ratelimited unlockWhen False _ -> do
      debug "Internal error (ratelimited but no headers), retrying"
      case rl of
        KnownRatelimit bucket ->
          void . P.embed . atomically $ modifyTVar' (bucket ^. #state) (#ongoing -~ 1)
        _ -> pure ()

      P.embed $ threadDelayUntil unlockWhen
      pure $ Retry (HTTPError 429 Nothing)
    Ratelimited unlockWhen True bs -> do
      debug "429 ratelimited globally"

      P.embed $ do
        atomically $ do
          case rl of
            KnownRatelimit bucket ->
              modifyTVar' (bucket ^. #state) (#ongoing -~ 1)
            _ -> pure ()
          case bs of
            Just (bs', bk) ->
              void $ updateBucket rlstate route bk bs'
            Nothing ->
              pure ()

        E.clear gl
        threadDelayUntil unlockWhen
        E.set gl
      pure $ Retry (HTTPError 429 Nothing)
    ServerError c -> do
      debug "Server failed, retrying"
      case rl of
        KnownRatelimit bucket ->
          P.embed $ useBucketOnce bucket
        _ -> debug "unknown ratelimit"
      pure $ Retry (HTTPError c Nothing)
    InternalResponseError c -> do
      debug "Internal error, retrying"
      case rl of
        KnownRatelimit bucket ->
          P.embed $ useBucketOnce bucket
        _ -> debug "unknown ratelimit"
      pure $ Retry (InternalClientError c)
    ClientError c v -> do
      case rl of
        KnownRatelimit bucket ->
          P.embed $ useBucketOnce bucket
        _ -> debug "unknown ratelimit"
      pure $ RFail (HTTPError c $ decode v)

doRequest :: BotC r => RateLimitState -> Route -> IO LbsResponse -> Sem r (Either RestError LB.ByteString)
doRequest rlstate route action =
  retryRequest
    5
    (doSingleRequest rlstate route (globalLock rlstate) action)
