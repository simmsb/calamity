{-# LANGUAGE NoImplicitPrelude #-}

module Calamity.LogEff
    ( LogEff
    , LogC ) where

import qualified Df1

import           DiPolysemy

import qualified Polysemy   as P

type LogEff = Di Df1.Level Df1.Path Df1.Message

type LogC r = P.Member LogEff r
