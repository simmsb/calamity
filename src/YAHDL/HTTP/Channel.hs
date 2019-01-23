-- | Channel endpoints

{-# LANGUAGE TypeApplications #-}

module YAHDL.HTTP.Channel where

import           Data.Aeson
import           Network.Wreq

import           YAHDL.HTTP.Request
import           YAHDL.HTTP.Route
import           YAHDL.Types.General
import           YAHDL.Types.Snowflake

data ChannelRequest a where
  CreateMessage :: Snowflake Channel -> Text -> {- TODO: embed -} ChannelRequest Message

instance Request (ChannelRequest a) where
  type RespVal (ChannelRequest a) = a

  toRoute (CreateMessage id _) = mkRouteBuilder
    & (S "channels" !:!)
    & (ID @Channel !:!)
    & (S "messages" !:!)
    & giveID id
    & buildRoute

  toAction q@(CreateMessage _ t) = post (url q) (object ["content" .= t])
