# Calamity

A discord library for haskell

# Example

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

runCounterAtomic :: P.Member (P.Embed IO) r => P.Sem (Counter ': r) () -> P.Sem r ()
runCounterAtomic m = do
  var <- P.embed $ newTVarIO (0 :: Int)
  P.runAtomicStateTVar var $ P.reinterpret (\case
                                              GetCounter -> P.atomicState (\v -> (v + 1, v))) m

handleFailByLogging m = do
  r <- P.runFail m
  case r of
    Left e -> DiP.error (e ^. packed)
    _      -> pure ()

handleFailByPrinting m = do
  r <- P.runFail m
  case r of
    Left e -> P.embed $ print (show e)
    _      -> pure ()

info = DiP.info @Text
debug = DiP.info @Text

main :: IO ()
main = do
  P.runFinal . P.embedToFinal . handleFailByPrinting . runCounterAtomic . runCacheInMemory . runMetricsNoop $ runBotIO
    (BotToken "") $ do
      react @"messagecreate" $ \msg -> handleFailByLogging $ do
        when (msg ^. #content == "!count") $ replicateM_ 3 $ do
          val <- getCounter
          info $ "the counter is: " <> fromStrict (showt val)
          void . invokeRequest $ CreateMessage (msg ^. #channelID) ("The value is: " <> showt val)
        when (msg ^. #content == "!say hi") $ replicateM_ 3 . P.async $ do
          info "saying heya"
          Right msg' <- invokeRequest $ CreateMessage (msg ^. #channelID) "heya"
          info "sleeping"
          P.embed $ threadDelay (5 * 1000 * 1000)
          info "slept"
          void . invokeRequest $ EditMessage (msg ^. #channelID) msg' (Just "lol") Nothing
          info "edited"
        when (msg ^. #content == "!explode") $ do
          Just x <- pure Nothing
          debug "unreachable!"
        when (msg ^. #content == "!bye") $ do
          void . invokeRequest $ CreateMessage (msg ^. #channelID) "bye!"
          stopBot
```
