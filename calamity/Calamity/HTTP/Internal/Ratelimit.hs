{-# LANGUAGE TemplateHaskell #-}

-- | Module containing ratelimit stuff
module Calamity.HTTP.Internal.Ratelimit (
  newRateLimitState,
  doRequest,
  RatelimitEff (..),
  getRatelimitState,
) where

import Calamity.HTTP.Internal.Route
import Calamity.HTTP.Internal.Types
import Calamity.Internal.Utils
import Calamity.Types.LogEff
import Calamity.Types.TokenEff
import Control.Applicative
import Control.Concurrent
import Control.Concurrent.Event (Event)
import Control.Concurrent.Event qualified as E
import Control.Concurrent.STM
import Control.Exception.Safe qualified as Ex
import Control.Monad
import Data.Aeson qualified as Aeson
import Data.Aeson.Optics
import Data.ByteString qualified as B
import Data.ByteString.Lazy qualified as LB
import Data.Maybe
import Data.Text qualified as T
import Data.Time
import Data.Time.Clock.POSIX
import Network.HTTP.Client (responseStatus)
import Network.HTTP.Req
import Network.HTTP.Types
import Optics
import Optics.Operators.Unsafe ((^?!))
import Polysemy (Sem, makeSem)
import Polysemy qualified as P
import StmContainers.Map qualified as SC
import Prelude hiding (error)
import Prelude qualified

data RatelimitEff m a where
  GetRatelimitState :: RatelimitEff m RateLimitState

makeSem ''RatelimitEff

newRateLimitState :: IO RateLimitState
newRateLimitState = RateLimitState <$> SC.newIO <*> SC.newIO <*> E.newSet

data Ratelimit
  = KnownRatelimit Bucket
  | UnknownRatelimit RouteKey

getRateLimit :: RateLimitState -> RouteKey -> STM Ratelimit
getRateLimit s h = do
  bucketKey <- SC.lookup h $ bucketKeys s
  bucket <- join <$> traverse (`SC.lookup` buckets s) bucketKey
  case bucket of
    Just bucket' ->
      pure $ KnownRatelimit bucket'
    Nothing ->
      pure $ UnknownRatelimit h

mergeBucketStates :: BucketState -> BucketState -> BucketState
mergeBucketStates old new =
  new
    { ongoing = old ^. #ongoing
    , -- we only ignore the previous 'remaining' if we've not reset yet and the
      -- reset time has changed
      remaining =
        if isJust (old ^. #resetTime) && old ^. #resetKey /= new ^. #resetKey
          then min (old ^. #remaining) (new ^. #remaining)
          else new ^. #remaining
    , -- only take the new resetTime if it actually changed
      resetTime =
        if old ^. #resetKey /= new ^. #resetKey
          then new ^. #resetTime
          else old ^. #resetTime
    }

updateKnownBucket :: Bucket -> BucketState -> STM ()
updateKnownBucket bucket bucketState = modifyTVar' (bucket ^. #state) (`mergeBucketStates` bucketState)

{- | Knowing the bucket for a route, and the ratelimit info, map the route to
 the bucket key and retrieve the bucket
-}
updateBucket :: RateLimitState -> RouteKey -> B.ByteString -> BucketState -> STM Bucket
updateBucket s h b bucketState = do
  bucketKey <- SC.lookup h $ bucketKeys s
  case bucketKey of
    Just bucketKey' -> do
      -- if we know the bucket key here, then the bucket has already been made
      -- if the given bucket key is different than the known bucket key then oops
      bucket <- SC.lookup bucketKey' $ buckets s
      case bucket of
        Just bucket' -> do
          modifyTVar' (bucket' ^. #state) (`mergeBucketStates` bucketState)
          pure bucket'
        Nothing -> Prelude.error "Not possible"
    Nothing -> do
      -- we didn't know the key to this bucket, but we might know the bucket
      -- if we truly don't know the bucket, then make a new one
      bs <- do
        bucket <- SC.lookup b $ buckets s
        case bucket of
          Just bs -> pure bs
          Nothing -> do
            bs <- Bucket <$> newTVar bucketState
            SC.insert bs b $ buckets s
            pure bs

      SC.insert b h $ bucketKeys s
      pure bs

resetBucket :: Bucket -> STM ()
resetBucket bucket =
  modifyTVar'
    (bucket ^. #state)
    ( \bs ->
        bs
          & #remaining .~ bs ^. #limit
          & #resetTime .~ Nothing
    )

canResetBucketNow :: UTCTime -> BucketState -> Bool
canResetBucketNow _ BucketState {ongoing} | ongoing > 0 = False
-- don't allow resetting the bucket if there's ongoing requests, we'll wait
-- until another request finishes and updates the counter
canResetBucketNow now bs = case bs ^. #resetTime of
  Just rt -> now > rt
  Nothing -> False

-- canResetBucket :: BucketState -> Bool
-- canResetBucket bs = isNothing $ bs ^. #startedWaitingTime

shouldWaitForUnlock :: BucketState -> Bool
shouldWaitForUnlock BucketState {remaining = 0, ongoing} = ongoing > 0
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
              ( (#remaining %~ pred)
                  . (#ongoing %~ succ)
              )
            pure GoNow
          else do
            -- the bucket has expired, there are no ongoing requests because of
            -- [0] wait and then retry after we can unlock the bucket
            pure (intoWaitDelay $ s ^. #resetTime)

      case mWaitDelay of
        WaitUntil waitUntil -> do
          if waitUntil < now
            then threadDelayMS 20
            else -- if the reset is in the past, we're fucked
              threadDelayUntil waitUntil
          -- if we needed to sleep, go again so that multiple concurrent requests
          -- don't exceed the bucket, to ensure we don't sit in a loop if a
          -- request dies on us, bail out after 50 loops
          when (tries < 50) $ go (tries + 1) -- print "bailing after number of retries"
        WaitRetrySoon -> do
          threadDelayMS 20
          when (tries < 50) $ go (tries + 1) -- print "bailing after number of retries"
        GoNow -> do
          -- print "ok going forward with request"
          pure ()

doDiscordRequest :: P.Members '[RatelimitEff, TokenEff, LogEff, P.Embed IO] r => IO LbsResponse -> Sem r DiscordResponseType
doDiscordRequest r = do
  r'' <- P.embed $ Ex.catchAny (Right <$> r) (pure . Left . Ex.displayException)
  case r'' of
    Right r' -> do
      let status = responseStatus . toVanillaResponse $ r'
      if
          | statusIsSuccessful status -> do
              let resp = responseBody r'
              debug $ "Got good response from discord: " <> (T.pack . show $ status)
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
              error . T.pack $ "Something went wrong: " <> show err <> ", response: " <> show r'
              pure $ ClientError (statusCode status) err
          | otherwise -> do
              debug . T.pack $ "Got server error from discord: " <> (show . statusCode $ status)
              pure $ ServerError (statusCode status)
    Left e -> do
      error . T.pack $ "Something went wrong with the http client: " <> e
      pure . InternalResponseError $ T.pack e

-- | Parse a ratelimit header returning when it unlocks
parseRateLimitHeader :: HttpResponse r => UTCTime -> r -> Maybe UTCTime
parseRateLimitHeader now r = computedEnd <|> end
  where
    computedEnd :: Maybe UTCTime
    computedEnd = flip addUTCTime now <$> resetAfter

    resetAfter :: Maybe NominalDiffTime
    resetAfter = realToFrac <$> responseHeader r "X-Ratelimit-Reset-After" ^? _Just % _Double

    end :: Maybe UTCTime
    end =
      posixSecondsToUTCTime
        . realToFrac
        <$> responseHeader r "X-Ratelimit-Reset"
        ^? _Just
        % _Double

buildBucketState :: HttpResponse r => UTCTime -> r -> Maybe (BucketState, B.ByteString)
buildBucketState now r = (,) <$> bs <*> bucketKey
  where
    remaining = responseHeader r "X-RateLimit-Remaining" ^? _Just % _Integral
    limit = responseHeader r "X-RateLimit-Limit" ^? _Just % _Integral
    resetKey = ceiling <$> responseHeader r "X-RateLimit-Reset" ^? _Just % _Double
    resetTime = parseRateLimitHeader now r
    bs = BucketState resetTime <$> resetKey <*> remaining <*> limit <*> pure 0
    bucketKey = responseHeader r "X-RateLimit-Bucket"

-- | Parse the retry after field, returning when to retry
parseRetryAfter :: UTCTime -> Aeson.Value -> UTCTime
parseRetryAfter now r = addUTCTime retryAfter now
  where
    retryAfter = realToFrac $ r ^?! key "retry_after" % _Double

isGlobal :: Aeson.Value -> Bool
isGlobal r = r ^? key "global" % _Bool == Just True

-- Either (Either a a) b
data ShouldRetry a b
  = Retry a
  | RFail a
  | RGood b

retryRequest ::
  P.Members '[RatelimitEff, TokenEff, LogEff, P.Embed IO] r =>
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
          debug . T.pack $ "Request failed after " <> show maxRetries <> " retries"
          pure $ Left r
        Retry _ ->
          retryInner (numRetries + 1)
        RFail r -> do
          debug "Request failed due to error response"
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
  P.Members '[RatelimitEff, TokenEff, LogEff, P.Embed IO] r =>
  RateLimitState ->
  Route ->
  -- | Global lock
  Event ->
  -- | Request action
  IO LbsResponse ->
  Sem r (ShouldRetry RestError LB.ByteString)
doSingleRequest rlstate route gl r = do
  P.embed $ E.wait (globalLock rlstate)

  rl <- P.embed . atomically $ getRateLimit rlstate (routeKey route)

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
            modifyTVar' (bucket ^. #state) (#ongoing %~ pred)
          _ -> pure ()
        case (rl, rlHeaders) of
          (KnownRatelimit bucket, Just (bs, _bk)) ->
            updateKnownBucket bucket bs
          (_, Just (bs, bk)) ->
            void $ updateBucket rlstate (routeKey route) bk bs
          (_, Nothing) -> pure ()
      pure $ RGood v
    Ratelimited unlockWhen False (Just (bs, bk)) -> do
      debug . T.pack $ "429 ratelimited on route, retrying at " <> show unlockWhen

      P.embed . atomically $ do
        case rl of
          KnownRatelimit bucket -> do
            modifyTVar' (bucket ^. #state) (#ongoing %~ pred)
            updateKnownBucket bucket bs
          _ -> void $ updateBucket rlstate (routeKey route) bk bs

      P.embed $ do
        threadDelayUntil unlockWhen

      pure $ Retry (HTTPError 429 Nothing)
    Ratelimited unlockWhen False _ -> do
      debug "Internal error (ratelimited but no headers), retrying"
      case rl of
        KnownRatelimit bucket ->
          void . P.embed . atomically $ modifyTVar' (bucket ^. #state) (#ongoing %~ pred)
        _ -> pure ()

      P.embed $ threadDelayUntil unlockWhen
      pure $ Retry (HTTPError 429 Nothing)
    Ratelimited unlockWhen True bs -> do
      debug "429 ratelimited globally"

      P.embed $ do
        atomically $ do
          case rl of
            KnownRatelimit bucket ->
              modifyTVar' (bucket ^. #state) (#ongoing %~ pred)
            _ -> pure ()
          case bs of
            Just (bs', bk) ->
              void $ updateBucket rlstate (routeKey route) bk bs'
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
      pure $ RFail (HTTPError c $ Aeson.decode v)

doRequest :: P.Members '[RatelimitEff, TokenEff, LogEff, P.Embed IO] r => RateLimitState -> Route -> IO LbsResponse -> Sem r (Either RestError LB.ByteString)
doRequest rlstate route action =
  retryRequest
    5
    (doSingleRequest rlstate route (globalLock rlstate) action)
