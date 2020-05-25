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
import           Calamity.Commands
import qualified Calamity.Commands.Context                  as CommandContext
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

info, debug :: BotC r => Text -> P.Sem r ()
info = DiP.info
debug = DiP.info

tellt :: (BotC r, Tellable t) => t -> Text -> P.Sem r (Either RestError Message)
tellt t m = tell t $ L.toStrict m

main :: IO ()
main = do
  token <- view packed <$> getEnv "BOT_TOKEN"
  void . P.runFinal . P.embedToFinal . runCounterAtomic . runCacheInMemory . runMetricsPrometheusIO . useConstantPrefix "!"
    $ runBotIO (BotToken token) $ do
    addCommands $ do
      helpCommand
      command @'[User] "utest" $ \ctx u -> do
        void $ tellt ctx $ "got user: " <> showtl u
      command @'[Named "u" User, Named "u1" User] "utest2" $ \ctx u u1 -> do
        void $ tellt ctx $ "got user: " <> showtl u <> "\nand: " <> showtl u1
      command @'[L.Text, Snowflake User] "test" $ \ctx something aUser -> do
        info $ "something = " <> showt something <> ", aUser = " <> showt aUser
      command @'[] "hello" $ \ctx -> do
        void $ tellt ctx "heya"
      group "testgroup" $ do
        command @'[[L.Text]] "test" $ \ctx l -> do
          void $ tellt ctx ("you sent: " <> showtl l)
        command @'[] "count" $ \ctx -> do
          val <- getCounter
          void $ tellt ctx ("The value is: " <> showtl val)
        group "say" $ do
          command @'[KleeneConcat L.Text] "this" $ \ctx msg -> do
            void $ tellt ctx msg
      command @'[Snowflake Emoji] "etest" $ \ctx e -> do
        void $ tellt ctx $ "got emoji: " <> showtl e
      command @'[] "explode" $ \ctx -> do
        Just x <- pure Nothing
        debug "unreachable!"
      command @'[] "bye" $ \ctx -> do
        void $ tellt ctx "bye!"
        stopBot
      command @'[] "fire-evt" $ \ctx -> do
        fire $ customEvt @"my-event" ("aha" :: L.Text, ctx ^. #message)
      command @'[L.Text] "wait-for" $ \ctx s -> do
        void $ tellt ctx ("waiting for !" <> s)
        waitUntil @'MessageCreateEvt (\msg -> msg ^. #content == ("!" <> s))
        void $ tellt ctx ("got !" <> s)
    react @'MessageCreateEvt $ \msg -> handleFailByLogging $ case msg ^. #content of
      "!say hi" -> replicateM_ 3 . P.async $ do
        info "saying heya"
        Right msg' <- tellt msg "heya"
        info "sleeping"
        P.embed $ threadDelay (5 * 1000 * 1000)
        info "slept"
        void . invoke $ EditMessage (msg ^. #channelID) msg' (Just "lol") Nothing
        info "edited"
      _ -> pure ()
    react @('CustomEvt "command-error" (CommandContext.Context, CommandError)) $ \(ctx, e) -> do
      info $ "Command failed with reason: " <> showt e
      case e of
        ParseError n r -> void . tellt ctx $ "Failed to parse parameter: `" <> L.fromStrict n <> "`, with reason: ```\n" <> r <> "```"
    react @('CustomEvt "my-event" (L.Text, Message)) $ \(s, m) ->
      void $ tellt m ("Somebody told me to tell you about: " <> s)
```
