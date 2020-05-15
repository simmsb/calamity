<h1 align="center">Calamity</h1>

<!-- [![Hackage](https://img.shields.io/hackage/v/calamity)](https://hackage.haskell.org/package/calamity) -->
<!-- [![Gitlab pipeline status](https://img.shields.io/gitlab/pipeline/nitros12/calamity)](https://gitlab.com/nitros12/calamity/pipelines) -->
<!-- [![License](https://img.shields.io/github/license/nitros12/calamity)](https://github.com/nitros12/calamity/blob/master/LICENSE) -->
<!-- [![Hackage-Deps](https://img.shields.io/hackage-deps/v/calamity)](https://hackage.haskell.org/package/calamity) -->

<p align="center">
  <a href="https://hackage.haskell.org/package/calamity"><img src="https://img.shields.io/hackage/v/calamity" alt="Hackage"></a>
  <a href="https://gitlab.com/nitros12/calamity/pipelines"><img src="https://img.shields.io/gitlab/pipeline/nitros12/calamity" alt="Gitlab pipeline status"></a>
  <a href="https://github.com/nitros12/calamity/blob/master/LICENSE"><img src="https://img.shields.io/github/license/nitros12/calamity" alt="License"></a>
  <a href="https://hackage.haskell.org/package/calamity"><img src="https://img.shields.io/hackage-deps/v/calamity" alt="Hackage-Deps"></a>
</p>

Calamity is a Haskell library for writing discord bots, it uses
[Polysemy](https://hackage.haskell.org/package/polysemy) as the core library for
handling effects, allowing you to pick and choose how to handle certain features
of the library.

The current customisable effects are:

* Cache: The default cache handler keeps the cache in memory, however you could
  write a cache handler that stores cache in a database for example.

* Metrics: The library has counters, gauges, and histograms installed to measure
  useful things, by default these are not used (and cost nothing), but could be
  combined with [Prometheus](https://hackage.haskell.org/package/prometheus). An
  example of using prometheus as the metrics handler can be found
  [here](https://github.com/nitros12/calamity-example).

# Docs

You can find documentation on hackage: [here](https://hackage.haskell.org/package/calamity)

# Example

An example project can be found at:
[nitros12/calamity-example](https://github.com/nitros12/calamity-example)

``` haskell
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

{-# LANGUAGE TypeOperators #-}

module Main where

import           Calamity
import           Calamity.Cache.InMemory
import           Calamity.Metrics.Noop

import           Control.Concurrent
import           Control.Concurrent.STM.TVar
import           Control.Lens
import           Control.Monad

import           Data.Text.Lazy              ( Text, fromStrict )
import           Data.Text.Strict.Lens

import qualified DiPolysemy                  as DiP

import qualified Polysemy                    as P
import qualified Polysemy.Async              as P
import qualified Polysemy.AtomicState        as P
import qualified Polysemy.Embed              as P
import qualified Polysemy.Fail               as P

import           Prelude                     hiding ( error )

import           TextShow

data Counter m a where
  GetCounter :: Counter m Int

P.makeSem ''Counter

runCounterAtomic :: P.Member (P.Embed IO) r => P.Sem (Counter ': r) a -> P.Sem r a
runCounterAtomic m = do
  var <- P.embed $ newTVarIO (0 :: Int)
  P.runAtomicStateTVar var $ P.reinterpret (\case
                                              GetCounter -> P.atomicState (\v -> (v + 1, v))) m

handleFailByLogging m = do
  r <- P.runFail m
  case r of
    Left e -> DiP.error (e ^. packed)
    _      -> pure ()

info = DiP.info @Text
debug = DiP.info @Text

tellt :: (BotC r, Tellable t) => t -> Text -> P.Sem r (Either RestError Message)
tellt = tell @Text

main :: IO ()
main = do
  token <- view packed <$> getEnv "BOT_TOKEN"
  void . P.runFinal . P.embedToFinal . runCounterAtomic . runCacheInMemory . runMetricsPrometheusIO
    $ runBotIO (BotToken token) $ do
    react @'MessageCreateEvt $ \msg -> handleFailByLogging $ case msg ^. #content of
      "!count" -> replicateM_ 3 $ do
        val <- getCounter
        info $ "the counter is: " <> showt val
        void $ tellt msg ("The value is: " <> showt val)
      "!say hi" -> replicateM_ 3 . P.async $ do
        info "saying heya"
        Right msg' <- tellt msg "heya"
        info "sleeping"
        P.embed $ threadDelay (5 * 1000 * 1000)
        info "slept"
        void . invoke $ EditMessage (msg ^. #channelID) msg' (Just "lol") Nothing
        info "edited"
      "!explode" -> do
        Just x <- pure Nothing
        debug "unreachable!"
      "!bye" -> do
        void $ tellt msg "bye!"
        stopBot
      "!fire-evt" -> fire $ customEvt @"my-event" ("aha" :: Text, msg)
      "!wait" -> do
        void $ tellt msg "waiting for !continue"
        waitUntil @'MessageCreateEvt (\msg -> (debug $ "got message: " <> showt msg) >> (pure $ msg ^. #content == "!continue"))
        void $ tellt msg "got !continue"
      _ -> pure ()
    react @('CustomEvt "my-event" (Text, Message)) $ \(s, m) ->
      void $ tellt m ("Somebody told me to tell you about: " <> s)
```
