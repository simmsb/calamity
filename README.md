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
     , calamity >= 0.3.0.0
     , text >= 1.2 && < 2.1
     , lens >= 5.1 && < 6
     , di-polysemy ^>= 0.2
     , di >= 1.3 && < 2
     , df1 >= 0.3 && < 0.5
     , di-core ^>= 1.0.4
     , polysemy >= 1.5 && <2
     , polysemy-plugin >= 0.3 && <0.5
     , stm >= 2.5 && <3
     , text-show >= 3.8 && <4
-}

{-# OPTIONS_GHC -fplugin=Polysemy.Plugin #-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedLabels #-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE BlockArguments #-}

{-# LANGUAGE ApplicativeDo #-}

module Main (main) where

import Calamity
import Calamity.Cache.InMemory
import Calamity.Commands
import Calamity.Commands.Context (useFullContext)
import qualified Calamity.Interactions as I
import Calamity.Metrics.Noop
import Control.Concurrent
import Control.Lens
import Control.Monad
import qualified Data.Text as T
import qualified Di
import qualified DiPolysemy as DiP
import GHC.Generics
import qualified Polysemy as P
import qualified Polysemy.Async as P
import qualified Polysemy.State as P
import System.Environment (getEnv)
import TextShow

data MyViewState = MyViewState
  { numOptions :: Int
  , selected :: Maybe T.Text
  }
  deriving (Generic)

main :: IO ()
main = do
  token <- T.pack <$> getEnv "BOT_TOKEN"
  Di.new $ \di ->
    void . P.runFinal . P.embedToFinal . DiP.runDiToIO di
      . runCacheInMemory
      . runMetricsNoop
      . useConstantPrefix "!"
      . useFullContext
      $ runBotIO (BotToken token) defaultIntents $ do
        addCommands $ do
          -- just some examples

          command @'[User] "utest" \ctx u -> do
            void . tell @T.Text ctx $ "got user: " <> showt u
          command @'[Named "u" User, Named "u1" User] "utest2" \ctx u u1 -> do
            void . tell @T.Text ctx $ "got user: " <> showt u <> "\nand: " <> showt u1
          command @'[T.Text, Snowflake User] "test" \_ctx something aUser -> do
            DiP.info $ "something = " <> showt something <> ", aUser = " <> showt aUser
          group "testgroup" $ do
            void $ command @'[[T.Text]] "test" \ctx l -> do
              void . tell @T.Text ctx $ "you sent: " <> showt l
            group "say" do
              command @'[KleenePlusConcat T.Text] "this" \ctx msg -> do
                void $ tell @T.Text ctx msg
          command @'[] "explode" \_ctx -> do
            Just _ <- pure Nothing
            DiP.debug @T.Text "unreachable!"
          command @'[] "bye" \ctx -> do
            void $ tell @T.Text ctx "bye!"
            stopBot

          -- views!

          command @'[] "components" \ctx -> do
            let view options = do
                  ~(add, done) <- I.row do
                    add <- I.button ButtonPrimary "add"
                    done <- I.button ButtonPrimary "done"
                    pure (add, done)
                  s <- I.select options
                  pure (add, done, s)
            let initialState = MyViewState 1 Nothing
            s <- P.evalState initialState $
              I.runView (view ["0"]) (tell ctx) \(add, done, s) -> do
                when add do
                  n <- P.gets (^. #numOptions)
                  let n' = n + 1
                  P.modify' (#numOptions .~ n')
                  let options = map (T.pack . show) [0 .. n]
                  I.replaceView (view options) (void . I.edit)

                when done do
                  finalSelected <- P.gets (^. #selected)
                  I.endView finalSelected
                  I.deleteInitialMsg
                  void . I.respond $ case finalSelected of
                    Just x -> "Thanks: " <> x
                    Nothing -> "Oopsie"

                case s of
                  Just s' -> do
                    P.modify' (#selected ?~ s')
                    void I.deferComponent
                  Nothing -> pure ()
            P.embed $ print s

          -- more views!

          command @'[] "cresponses" \ctx -> do
            let view = I.row do
                  a <- I.button ButtonPrimary "defer"
                  b <- I.button ButtonPrimary "deferEph"
                  c <- I.button ButtonPrimary "deferComp"
                  d <- I.button ButtonPrimary "modal"
                  pure (a, b, c, d)

                modalView = do
                  a <- I.textInput TextInputShort "a"
                  b <- I.textInput TextInputParagraph "b"
                  pure (a, b)

            I.runView view (tell ctx) $ \(a, b, c, d) -> do
              when a do
                void I.defer
                P.embed $ threadDelay 1000000
                void $ I.followUp @T.Text "lol"

              when b do
                void I.deferEphemeral
                P.embed $ threadDelay 1000000
                void $ I.followUpEphemeral @T.Text "lol"

              when c do
                void I.deferComponent
                P.embed $ threadDelay 1000000
                void $ I.followUp @T.Text "lol"

              when d do
                void . P.async $ do
                  I.runView modalView (void . I.pushModal "lol") $ \(a, b) -> do
                    P.embed $ print (a, b)
                    void $ I.respond ("Thanks: " <> a <> " " <> b)
                    I.endView ()

              pure ()
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
