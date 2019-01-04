-- | Module containing ratelimit stuff

module YAHDL.HTTP.Ratelimit
  ( RateLimitState(..)
  , newRateLimitState
  , getRateLimit
  )
where

import           Control.Concurrent.STM
import           Control.Concurrent.STM.Lock    ( Lock )
import qualified Control.Concurrent.STM.Lock   as L
import           Control.Retry
import           Focus
import qualified StmContainers.Map             as SC

import           YAHDL.HTTP.Route
import           YAHDL.Types.General            ( Token
                                                , formatToken
                                                )

data RateLimitState = RateLimitState
  { rateLimits :: SC.Map Route Lock
  , globalLock :: Lock
  } deriving (Generic)

newRateLimitState :: STM RateLimitState
newRateLimitState = RateLimitState <$> SC.new <*> L.new

getRateLimit :: RateLimitState -> Route -> STM Lock
getRateLimit s h = SC.focus (lookupWithDefaultM L.new) h (rateLimits s)

-- TODO: routes with hashes (just steal from haskord :^^^))
-- TODO: bot state reader (token, rl states, etc)

-- TODO: write functions for each method
-- TODO: write types to contain parameters/ data for each route
