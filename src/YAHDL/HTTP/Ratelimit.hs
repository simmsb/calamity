-- | Module containing ratelimit stuff

module YAHDL.HTTP.Ratelimit where

import           Control.Concurrent.STM
import           Control.Concurrent.STM.Lock    ( Lock )
import qualified Control.Concurrent.STM.Lock   as L
import qualified StmContainers.Map             as SC
import           Focus
import           Control.Retry
import           Network.Wreq

import           YAHDL.HTTP.Route               ( Route
                                                , RouteBuilder
                                                )
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

defaultRequestOptions :: Options
defaultRequestOptions =
  defaults & header "User-Agent" .~ ["YAHDL (https://github.com/nitros12/yet-another-haskell-discord-library)"]
           & checkResponse ?~ (\_ _ -> pure ())

requestOptions :: Token -> Options
requestOptions t =
  defaultRequestOptions & header "Authorization" .~ [formatToken t]

-- TODO: write functions for each method
-- TODO: write types to contain parameters/ data for each route
