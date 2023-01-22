{-# LANGUAGE NoImplicitPrelude #-}

-- | The logging effect we use
module Calamity.Types.LogEff (
  LogEff,
  LogC,
) where

import Df1 qualified

import DiPolysemy

import Polysemy qualified as P

type LogEff = Di Df1.Level Df1.Path Df1.Message

type LogC r = P.Member LogEff r
