<h1 align="center">Calamity Commands</h1>

<p align="center">
  <a href="https://hackage.haskell.org/package/calamity-commands"><img src="https://img.shields.io/hackage/v/calamity-commands" alt="Hackage"></a>
  <a href="https://gitlab.com/simmsb/calamity/pipelines"><img src="https://img.shields.io/gitlab/pipeline/simmsb/calamity" alt="Gitlab pipeline status"></a>
  <a href="https://github.com/simmsb/calamity/blob/master/LICENSE"><img src="https://img.shields.io/github/license/simmsb/calamity" alt="License"></a>
  <a href="https://hackage.haskell.org/package/calamity"><img src="https://img.shields.io/hackage-deps/v/calamity" alt="Hackage-Deps"></a>
  <a href="https://discord.gg/NGCThCY"><img src="https://discord.com/api/guilds/754446998077178088/widget.png?style=shield" alt="Discord Invite"></a>
</p>

Calamity Commands is a Haskell library for constructing text-based commands, it
uses [Polysemy](https://hackage.haskell.org/package/polysemy) as the core
library for handling effects, allowing you to pick and choose how to handle
certain features of the library.

# Docs

You can find documentation on hackage at: https://hackage.haskell.org/package/calamity-commands

# Examples

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

import Polysemy
import CalamityCommands.Commands
import CalamityCommands.Commands.Help
import CalamityCommands.Commands.Context
import CalamityCommands.Commands.ParsePrefix
import Data.Functor.Identity
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy.IO as LT

-- Make a command handler, we don't actually use the context and therefore this
-- handler is generic over the context used
h' :: CommandContext Identity c (Either LT.Text Int) => CommandHandler Identity c (Either LT.Text Int)
h' = runIdentity . runFinal $ do
  (h, _) <- buildCommands $ do
        command @'[Int, Int] "add" $ \ctx a b -> pure $ Right (a + b)
        command @'[Int, Int] "mul" $ \ctx a b -> pure $ Right (a * b)
        helpCommand (pure . Left)
  pure h

-- To use the commands we need to provide the interpreters for
-- 'ConstructContext' and 'ParsePrefix', the default provided ones are being
-- used here: 'useBasicContext' which makes @ctx ~ 'BasicContext'@, and
-- @'useConstantPrefix' "!"@ which treats any input starting with @!@ as a
-- command.
--
--
-- The 'processCommands' function can then be used to parse and invoke commands,
-- since commands are generic over the monad they run in we use @'runIdentity' .
-- 'runFinal' . 'embedToFinal'@ to have the commands interpreted purely.
--
--
-- This function 'r' takes an input string such as "!add 1 2", and then looks up
-- the invoked command and runs it, returning the result.
r :: LT.Text -> Maybe (Either
                       (CmdInvokeFailReason (BasicContext Identity (Either LT.Text Int)))
                       (BasicContext Identity (Either LT.Text Int), Either LT.Text Int))
r = runIdentity . runFinal . embedToFinal . useBasicContext . useConstantPrefix "!" . processCommands h'

-- Then to display the result of processing the command nicely, we can use a
-- something like this function, which prints the result of a command if one was
-- invoked successfully, and prints the error nicely if not.
rm :: LT.Text -> IO ()
rm s = case r s of
            Just (Right (_, Right r)) ->
              print r

            Just (Right (_, Left h)) ->
              LT.putStrLn h

            Just (Left (CommandInvokeError _ (ParseError t r))) ->
              LT.putStrLn ("Parsing parameter " <> LT.fromStrict t <> " failed with reason: " <> r)

            _ -> pure ()
```

