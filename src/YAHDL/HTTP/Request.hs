-- | Generic Request type

module YAHDL.HTTP.Request where

import           Data.Text.Strict.Lens

import           Data.String                    ( String )
import           YAHDL.HTTP.Route

class Request a where
  type RespVal a

  toRoute :: a -> Route

  url :: a -> String
  url r = path (toRoute r) ^. unpacked

  invokeRequest :: a -> RespVal a -- TODO: bot monad goes here
