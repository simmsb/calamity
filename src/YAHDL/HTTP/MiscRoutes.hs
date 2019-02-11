-- | Miscellaneous routes

{-# LANGUAGE TypeApplications #-}

module YAHDL.HTTP.MiscRoutes where

import           Network.Wreq

import           YAHDL.HTTP.Request
import           YAHDL.HTTP.Route
import           YAHDL.HTTP.Types

data MiscRequest a where
  GetGatewayBot :: MiscRequest BotGatewayResponse

instance Request (MiscRequest a) a where
  toRoute GetGatewayBot =
    mkRouteBuilder !:! S "gateway" !:! S "bot"
    & buildRoute

  toAction q@GetGatewayBot opts = getWith opts (url q)
