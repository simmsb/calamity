-- | Module containing ratelimit stuff

module YAHDL.HTTP.Ratelimit where

import           GHC.Generics
import           Control.Concurrent.STM
import           Control.Concurrent.STM.Lock    ( Lock )
import qualified Control.Concurrent.STM.Lock   as L
import qualified StmContainers.Map             as SC
import           Focus

data RateLimitState = RateLimitState
  { rateLimits :: SC.Map Route Lock
  , globalLock :: Lock
  } deriving (Generic)

newRateLimitState :: STM RateLimitState
newRateLimitState = RateLimitState <$> SC.new <*> L.new

type Route = Int -- TODO: fix this

getRateLimit :: RateLimitState -> Route -> STM Lock
getRateLimit s h = SC.focus (lookupWithDefaultM L.new) h (rateLimits s)


noExceptionResponseChecker :: a -> b -> IO ()
noExceptionResponseChecker _ _ = pure ()

-- TODO: routes with hashes (just steal from haskord :^^^))
-- TODO: bot state reader (token, rl states, etc)
