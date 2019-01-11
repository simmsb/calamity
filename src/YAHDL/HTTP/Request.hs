-- | Generic Request type

module YAHDL.HTTP.Request where

import           Data.String                    ( String )
import           Data.Text.Strict.Lens
import           Network.Wreq

import           YAHDL.HTTP.Route
import           YAHDL.Types.General
import           YAHDL.Client.Types


class Request a where
  type RespVal a

  toRoute :: a -> Route

  url :: a -> String
  url r = path (toRoute r) ^. unpacked

  invokeRequest :: a -> BotM (RespVal a)


defaultRequestOptions :: Options
defaultRequestOptions =
  defaults & header "User-Agent" .~ ["YAHDL (https://github.com/nitros12/yet-another-haskell-discord-library)"]
           & checkResponse ?~ (\_ _ -> pure ())

requestOptions :: Token -> Options
requestOptions t =
  defaultRequestOptions & header "Authorization" .~ [formatToken t]
