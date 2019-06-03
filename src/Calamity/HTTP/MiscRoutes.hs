-- | Miscellaneous routes
module Calamity.HTTP.MiscRoutes where

import           Calamity.HTTP.Request
import           Calamity.HTTP.Route
import           Calamity.HTTP.Types

import           Network.Wreq

data MiscRequest a where
  GetGateway :: MiscRequest GatewayResponse
  GetGatewayBot :: MiscRequest BotGatewayResponse

instance Request (MiscRequest a) a where
  toRoute GetGateway = mkRouteBuilder // S "gateway"
    & buildRoute

  toRoute GetGatewayBot = mkRouteBuilder // S "gateway" // S "bot"
    & buildRoute

  toAction q@GetGateway opts = getWith opts (url q)
  toAction q@GetGatewayBot opts = getWith opts (url q)
