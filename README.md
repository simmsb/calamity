# Calamity

A discord library for haskell

# Example

``` haskell
{-# LANGUAGE OverloadedStrings #-}

import           Calamity.Client
import           Calamity.HTTP
import           Calamity.Types.General

import           Data.Default
import           Data.Text.Strict.Lens

import qualified Polysemy               as P
import qualified Polysemy.Fail          as P


handleErrorByLogging m = do
  r <- P.runFail m
  case r of
    Left e ->
      error (e ^. packed)
    _ -> pure ()

main :: IO ()
main = do
  P.runFinal . P.embedToFinal $ runBotIO (BotToken "token") $ do
    react @"messagecreate" $ \msg state -> handleErrorByLogging $ do
      when (msg ^. #content == "!say hi") $ do
        Right msg' <- invokeRequest $ CreateMessage (msg ^. #channelID) "heya"
        P.embed $ threadDelay 1000
        Right msg'' <- invokeRequest $ EditMessage (msg ^. #channelID) msg' (Just "lol") Nothing
```
