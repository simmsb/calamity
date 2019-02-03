-- | Miscellaneous routes

{-# LANGUAGE TypeApplications #-}

module YAHDL.HTTP.MiscRoutes where

import           Network.Wreq

import           YAHDL.HTTP.Request
import           YAHDL.HTTP.Route

data MiscRequest a where
  GetGateway :: MiscRequest Text

instance Request (MiscRequest a) a where
  toRoute GetGateway =
    mkRouteBuilder !:! S "gateway" !:! S "bot"
    & buildRoute

  toAction q@GetGateway opts = getWith opts (url q)
