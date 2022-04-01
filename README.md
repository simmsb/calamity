<h1 align="center">Calamity</h1>

<p align="center">
  <a href="https://hackage.haskell.org/package/calamity"><img src="https://img.shields.io/hackage/v/calamity" alt="Hackage"></a>
  <a href="https://github.com/simmsb/calamity/actions/workflows/build.yml"><img src="https://github.com/simmsb/calamity/actions/workflows/build.yml/badge.svg" alt="Build Status"></a>
  <a href="https://github.com/simmsb/calamity/blob/master/LICENSE"><img src="https://img.shields.io/github/license/simmsb/calamity" alt="License"></a>
  <a href="https://hackage.haskell.org/package/calamity"><img src="https://img.shields.io/hackage-deps/v/calamity" alt="Hackage-Deps"></a>
  <a href="https://discord.gg/NGCThCY"><img src="https://discord.com/api/guilds/754446998077178088/widget.png?style=shield" alt="Discord Invite"></a>
</p>

Calamity is a Haskell library for writing discord bots, it uses
[Polysemy](https://hackage.haskell.org/package/polysemy) as the core library for
handling effects, allowing you to pick and choose how to handle certain features
of the library.

If you're looking for something with a less complicated interface, you might
want to take a look at
[discord-haskell](https://github.com/aquarial/discord-haskell).

The current customisable effects are:

* Cache: The default cache handler keeps the cache in memory, however you could
  write a cache handler that stores cache in a database for example.

* Metrics: The library has counters, gauges, and histograms installed to measure
  useful things, by default these are not used (and cost nothing), but could be
  combined with [Prometheus](https://hackage.haskell.org/package/prometheus). An
  example of using prometheus as the metrics handler can be found
  [here](https://github.com/simmsb/calamity-example).

* Logging: The [di-polysemy](https://hackage.haskell.org/package/di-polysemy)
  library is used to allow the logging effect to be customized, or disabled.

# Docs

You can find documentation on hackage at: https://hackage.haskell.org/package/calamity

There's also a good blog post that covers the fundamentals of writing a bot with
the library, you can read it here:
https://morrowm.github.io/posts/2021-04-29-calamity.html

# Examples

Here's a list of projects that use calamity:
<!-- - [simmsb/calamity-example](https://github.com/simmsb/calamity-example): An extended example of the snippet below, shows use of metrics. -->
- [simmsb/calamity-bot](https://github.com/simmsb/calamity-bot): Uses a database, showing modularization of groups/commands.
- [MorrowM/pandabot-discord](https://github.com/MorrowM/pandabot-discord): Uses a database, performs member role management, etc.
- [MorrowM/calamity-tutorial](https://github.com/MorrowM/calamity-tutorial): A bare minimum bot.
- [koluacik/gundyr](https://github.com/koluacik/gundyr): An admin bot that does role assignment, etc.

(Feel free to contact me via the discord server, or email me via
ben@bensimms.moe if you've written a bot using calamity, or don't want your
project listed here)

``` haskell
#!/usr/bin/env cabal
{- cabal:
  build-depends:
     base >= 4.13 && < 5
     , calamity >= 0.1.30.1
     , text >= 1.2 && < 2
     , lens >= 4.18 && < 5
     , di-polysemy ^>= 0.2
     , di >= 1.3 && < 2
     , df1 >= 0.3 && < 0.5
     , di-core ^>= 1.0.4
     , polysemy ^>= 1.5
     , polysemy-plugin ^>= 0.3
     , stm ^>= 2.5
     , text-show ^>= 3.9
-}

{-# OPTIONS_GHC -fplugin=Polysemy.Plugin #-}

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

{-# LANGUAGE BlockArguments #-}

{-# LANGUAGE TypeOperators #-}

module Main where

import           Calamity
import           Calamity.Cache.InMemory
import Calamity.Commands
import Calamity.Commands.Context (FullContext, useFullContext)
import           Calamity.Metrics.Noop

import           Control.Concurrent
import           Control.Concurrent.STM.TVar
import           Control.Lens
import           Control.Monad

import qualified Data.Text as T

import qualified Di
import qualified DiPolysemy                  as DiP

import qualified Polysemy                    as P
import qualified Polysemy.Async              as P
import qualified Polysemy.AtomicState        as P
import qualified Polysemy.Embed              as P
import qualified Polysemy.Fail               as P

import           Prelude                     hiding ( error )

import           System.Environment          (getEnv)

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
    Left e -> DiP.error $ T.pack e
    _      -> pure ()

info, debug :: BotC r => T.Text -> P.Sem r ()
info = DiP.info
debug = DiP.info

tellt :: (BotC r, Tellable t) => t -> T.Text -> P.Sem r (Either RestError Message)
tellt t m = tell t $ T.toStrict m

data MyCustomEvt = MyCustomEvt T.Text Message

main :: IO ()
main = do
  token <- T.pack <$> getEnv "BOT_TOKEN"
  Di.new $ \di ->
    void . P.runFinal . P.embedToFinal . DiP.runDiToIO di . runCounterAtomic 
         . runCacheInMemory . runMetricsNoop . useConstantPrefix "!" . useFullContext
      $ runBotIO (BotToken token) defaultIntents $ do
      addCommands $ do
        helpCommand
        command @'[User] "utest" $ \ctx u -> do
          void $ tellt ctx $ "got user: " <> showt u
        command @'[Named "u" User, Named "u1" User] "utest2" $ \ctx u u1 -> do
          void $ tellt ctx $ "got user: " <> showt u <> "\nand: " <> showt u1
        command @'[T.Text, Snowflake User] "test" $ \ctx something aUser -> do
          info $ "something = " <> showt something <> ", aUser = " <> showt aUser
        command @'[] "hello" $ \ctx -> do
          void $ tellt ctx "heya"
        group "testgroup" $ do
          command @'[[T.Text]] "test" $ \ctx l -> do
            void $ tellt ctx ("you sent: " <> showt l)
          command @'[] "count" $ \ctx -> do
            val <- getCounter
            void $ tellt ctx ("The value is: " <> showt val)
          group "say" $ do
            command @'[KleenePlusConcat T.Text] "this" $ \ctx msg -> do
              void $ tellt ctx msg
        command @'[Snowflake Emoji] "etest" $ \ctx e -> do
          void $ tellt ctx $ "got emoji: " <> showt e
        command @'[] "explode" $ \ctx -> do
          Just x <- pure Nothing
          debug "unreachable!"
        command @'[] "bye" $ \ctx -> do
          void $ tellt ctx "bye!"
          stopBot
        command @'[] "fire-evt" $ \ctx -> do
          fire . customEvt $ MyCustomEvt "aha" (ctx ^. #message)
        command @'[T.Text] "wait-for" $ \ctx s -> do
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
          void . invoke $ EditMessage (msg ^. #channelID) msg' (editMessageContent $ Just "lol")
          info "edited"
        _ -> pure ()
      react @('CustomEvt (CtxCommandError FullContext)) \(CtxCommandError ctx e) -> do
        info $ "Command failed with reason: " <> showt e
        case e of
          ParseError n r -> void . tellt ctx $ "Failed to parse parameter: `" <> T.fromStrict n <> "`, with reason: ```\n" <> r <> "```"
      react @('CustomEvt MyCustomEvt) $ \(MyCustomEvt s m) ->
        void $ tellt m ("Somebody told me to tell you about: " <> s)
```

## Disabling library logging

The library logs on debug levels by default, if you wish to disable logging you
can do something along the lines of:

``` haskell
import qualified Di
import qualified Df1
import qualified Di.Core
import qualified DiPolysemy

filterDi :: Di.Core.Di l Di.Path m -> Di.Core.Di l Di.Path m
filterDi = Di.Core.filter (\_ p _ -> Df1.Push "calamity" `notElem` p)

Di.new $ \di ->
-- ...
  . runDiToIO di
  -- disable logs emitted by calamity
  . DiPolysemy.local filterDi
  . runBotIO
  -- ...
```


## Nix

If you trust me, I have a [cachix](https://www.cachix.org/) cache setup at
`simmsb-calamity`.

With cachix installed, you should be able to run `cachix use simmsb-calamity` to
add my cache to your list of caches.

You can also just manually add the substituter and public key:

```
substituters = https://simmsb-calamity.cachix.org
trusted-public-keys = simmsb-calamity.cachix.org-1:CQsXXpwKsjSVu0BJFT/JSvy1j6R7rMSW2r3cRQdcuQM= 
```

After this nix builds should just use the cache (I hope?)

For an example of a bot built using nix, take a look at:
[simmsb/calamity-bot](https://github.com/simmsb/calamity-bot)
