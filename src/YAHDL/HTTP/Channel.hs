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
  type RespVal (ChannelRequest a) = Maybe a

  toRoute (CreateMessage id _) = mkRouteBuilder
    & (S "channels" !:!)
    & (ID @Channel !:!)
    & (S "messages" !:!)
    & giveID id
    & buildRoute

  invokeRequest q@(CreateMessage _ t) = do
    r <- liftIO $ asJSON =<< post (url q) (object ["content" .= t])
    pure $ r ^? responseBody
