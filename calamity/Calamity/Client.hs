-- | Module containing the core client stuff
module Calamity.Client (
  module Calamity.Client.Client,
  module Calamity.Client.Types,

  -- * Client stuff
  -- $clientDocs
) where

import Calamity.Client.Client
import Calamity.Client.Types

{- $clientDocs

 This module provides the core components for running a discord client, along
 with abstractions for registering event handlers, firing custom events, and
 waiting on events.


 ==== Registered Metrics

     1. Counter: @"events_recieved" [type, shard]@

         Incremented for each event received, the @type@ parameter is the type
         of event (the name of the constructor in
         'Calamity.Gateway.DispatchEvents.DispatchData'), and @shard@ is the
         ID of the shard that received the event.

     2. Histogram: @"cache_update"@

         Tracks how long it takes to update the cache after recieving an event.

     3. Histogram: @"event_handle"@

         Tracks how long it takes to process an event handler for an event.


 ==== Examples

 A simple client that prints every message recieved:

 @
 'Control.Monad.void' . 'Polysemy.runFinal' . 'Polysemy.embedToFinal' . 'Calamity.Cache.InMemory.runCacheInMemory' . 'Calamity.Metrics.Noop.runMetricsNoop' $ 'runBotIO' ('Calamity.Types.Token.BotToken' token) $ do
   'react' \@\''MessageCreateEvt' $ \\msg -> 'Polysemy.embed' $ 'print' msg
 @
-}
