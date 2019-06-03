# Calamity

A discord library for haskell

# Example

``` haskell
{-# LANGUAGE OverloadedStrings #-}

import Calamity.Client
import Calamity.Types.General
import Calamity.HTTP.Request
import Calamity.HTTP.Channel

import Control.Lens

main :: IO ()
main = do
  runWithHandlers (BotToken "<token>") do
    react @"messagecreate" \msg -> do
      when (msg ^. #content == "!say hi") do
        msg <- invokeRequest $ CreateMessage (msg ^. #channelID) "heya"
        print $ "Got message back: " <> show msg

```
