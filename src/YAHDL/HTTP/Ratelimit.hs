-- | Module containing ratelimit stuff

module YAHDL.HTTP.Ratelimit where

import           GHC.Generics
import           Control.Concurrent.STM
import           Control.Concurrent.STM.Lock    ( Lock )
import qualified Control.Concurrent.STM.Lock   as L
import           Control.Concurrent.STM.TVar
import qualified StmContainers.Map             as SC
import Focus

data RateLimitState = RateLimitState
  { rateLimits :: TVar (SC.Map Route Lock)
  , globalLock :: Lock
  } deriving (Generic)

type Route = Int -- TODO: fix this

getRateLimit :: RateLimitState -> Route -> STM Lock
getRateLimit s h = do
  rls <- readTVar (rateLimits s)
  SC.focus (lookupWithDefaultM L.new) h rls


noExceptionResponseChecker :: a -> b -> IO ()
noExceptionResponseChecker _ _ = pure ()

-- TODO: routes with hashes (just steal from haskord :^^^))
-- TODO: bot state reader (token, rl states, etc)
