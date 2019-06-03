-- | Channel endpoints

{-# LANGUAGE TypeApplications #-}

module Calamity.HTTP.Channel where

import           Data.Aeson
import           Network.Wreq

import           Calamity.HTTP.Request
import           Calamity.HTTP.Route
import           Calamity.Types.General
import           Calamity.Types.Snowflake

data ChannelRequest a where
  CreateMessage :: Snowflake Channel -> Text -> {- TODO: embed -} ChannelRequest Message

instance Request (ChannelRequest a) a where
  toRoute (CreateMessage id _) =
    mkRouteBuilder // S "channels" // ID @Channel // S "messages"
    & giveID id
    & buildRoute

  toAction q@(CreateMessage _ t) opts = postWith opts (Calamity.HTTP.Request.url q) (object ["content" .= t])
