-- | Miscellaneous routes
module Calamity.HTTP.MiscRoutes where

import           Calamity.HTTP.Internal.Request
import           Calamity.HTTP.Internal.Route
import           Calamity.HTTP.Internal.Types

import           Data.Function

import           Network.Wreq

data MiscRequest a where
  GetGateway    :: MiscRequest GatewayResponse
  GetGatewayBot :: MiscRequest BotGatewayResponse

instance Request (MiscRequest a) a where
  toRoute GetGateway = mkRouteBuilder // S "gateway"
    & buildRoute

  toRoute GetGatewayBot = mkRouteBuilder // S "gateway" // S "bot"
    & buildRoute

  toAction GetGateway = getWith
  toAction GetGatewayBot = getWith
