-- | Miscellaneous routes
module Calamity.HTTP.MiscRoutes where

import Calamity.HTTP.Internal.Request
import Calamity.HTTP.Internal.Route
import Calamity.HTTP.Internal.Types
import Data.Function ((&))

data MiscRequest a where
  GetGateway :: MiscRequest GatewayResponse
  GetGatewayBot :: MiscRequest BotGatewayResponse

instance Request (MiscRequest a) where
  type Result (MiscRequest a) = a

  route GetGateway =
    mkRouteBuilder // S "gateway"
      & buildRoute
  route GetGatewayBot =
    mkRouteBuilder // S "gateway" // S "bot"
      & buildRoute

  action GetGateway = getWith
  action GetGatewayBot = getWith
