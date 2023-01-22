module Calamity.Gateway (
  module Calamity.Gateway.Shard,
  module Calamity.Gateway.Types,
  module Calamity.Gateway.Intents,

  -- * Gateway
  -- $gatewayDocs
) where

import Calamity.Gateway.Intents
import Calamity.Gateway.Shard
import Calamity.Gateway.Types

{- $gatewayDocs

 This module contains all the gateway related things.


 ==== Registered Metrics

     1. Gauge: @"active_shards"@

         Keeps track of how many shards are currently active.
-}
