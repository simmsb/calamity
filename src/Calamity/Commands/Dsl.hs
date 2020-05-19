{-# LANGUAGE RecursiveDo #-}

-- | A DSL for generating commands and groups
module Calamity.Commands.Dsl
    ( command'
    , command
    , help
    , requires
    , requires'
    , requiresPure
    , group ) where

import           Calamity.Commands.Check
import           Calamity.Commands.Command     hiding ( help )
import           Calamity.Commands.Context     hiding ( command )
import           Calamity.Commands.Error
import           Calamity.Commands.Group       hiding ( help )
import           Calamity.Commands.LocalWriter

import qualified Data.Text                     as S
import qualified Data.Text.Lazy                as L

import qualified Polysemy                      as P
import qualified Polysemy.Fail                 as P
import qualified Polysemy.Fixpoint             as P
import qualified Polysemy.Reader               as P

-- | Build a command with an already prepared invokation action
command'
  :: P.Members '[LocalWriter [Command],
                 P.Reader (Maybe Group),
                 P.Reader (Context -> L.Text),
                 P.Reader [Check],
                 P.Final IO] r
  => S.Text
  -> (Context -> Either CommandError a)
  -> ((Context, a) -> P.Sem (P.Fail ': r) ())
  -> P.Sem r Command
command' name parser cb = do
  parent <- P.ask @(Maybe Group)
  checks <- P.ask @[Check]
  help' <- P.ask @(Context -> L.Text)
  cmd <- buildCommand' name parent checks help' parser cb
  ltell [cmd]
  pure cmd

command :: forall ps a r.
        ( P.Members '[LocalWriter [Command],
                      P.Reader (Maybe Group),
                      P.Reader (Context -> L.Text),
                      P.Reader [Check],
                      P.Final IO] r,
          TypedCommandC ps a (P.Sem (P.Fail ': r) ()))
        => S.Text
        -> (Context -> CommandForParsers ps (P.Sem (P.Fail ': r) ()))
        -> P.Sem r Command
command name cmd = do
  parent <- P.ask @(Maybe Group)
  checks <- P.ask @[Check]
  help' <- P.ask @(Context -> L.Text)
  cmd' <- buildCommand @ps name parent checks help' cmd
  ltell [cmd']
  pure cmd'

help :: P.Member (P.Reader (Context -> L.Text)) r
     => (Context -> L.Text)
     -> P.Sem r a
     -> P.Sem r a
help = P.local . const

requires :: P.Member (P.Reader [Check]) r
         => [Check]
         -> P.Sem r a
         -> P.Sem r a
requires = P.local . (<>)

requires' :: P.Members '[P.Reader [Check], P.Final IO] r
          => S.Text
          -> (Context -> P.Sem r (Maybe L.Text))
          -> P.Sem r a
          -> P.Sem r a
requires' name cb m = do
  check <- buildCheck name cb
  requires [check] m

requiresPure :: P.Member (P.Reader [Check]) r
             => [(S.Text, Context -> Maybe L.Text)]
             -> P.Sem r a
             -> P.Sem r a
requiresPure checks = requires $ map (uncurry buildCheckPure) checks

group :: P.Members '[LocalWriter [Command],
                     LocalWriter [Group],
                     P.Reader (Maybe Group),
                     P.Reader (Context -> L.Text),
                     P.Reader [Check],
                     P.Fixpoint,
                     P.Final IO] r
         => S.Text
         -> P.Sem r a
         -> P.Sem r a
group name m = mdo
  parent <- P.ask @(Maybe Group)
  checks <- P.ask @[Check]
  help'  <- P.ask @(Context -> L.Text)
  let group' = Group name parent commands children help' checks
  (children, (commands, res)) <- llisten @[Group] $ llisten @[Command] $ P.local (const $ Just group') m
  ltell [group']
  pure res

