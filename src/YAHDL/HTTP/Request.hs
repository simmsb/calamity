-- | Generic Request type

module YAHDL.HTTP.Request where

import           Control.Monad.Log              ( formatter
                                                , defaultFormatter
                                                , localLogger
                                                , toLogStr
                                                , Level
                                                , FormattedTime
                                                , LogStr
                                                )
import           Data.Aeson              hiding ( Options )
import qualified Data.ByteString.Lazy          as LB
import           Data.String                    ( String )
import           Data.Text.Strict.Lens
import           Network.Wreq

import           YAHDL.Client.Types
import           YAHDL.HTTP.Ratelimit
import           YAHDL.HTTP.Route
import           YAHDL.HTTP.Types
import           YAHDL.Types.General


fromResult :: Monad m => Result a -> ExceptT RestError m a
fromResult (Success a) = pure a
fromResult (Error   _) = throwE DecodeError

extractRight :: Monad m => Either a b -> ExceptT a m b
extractRight (Left  a) = throwE a
extractRight (Right a) = pure a

class Request a r | a -> r where
  toRoute :: a -> Route

  url :: a -> String
  url r = path (toRoute r) ^. unpacked

  toAction :: a -> Options -> IO (Response LB.ByteString)

  invokeRequest :: FromJSON r => a -> BotM (Either RestError r)
  invokeRequest r = runExceptT inner
    where inner :: ExceptT RestError BotM r
          inner = do
            rlState' <- asks rlState
            token' <- asks token

            resp <- localLogger withRequestLog $ doRequest rlState' (toRoute r) (toAction r $ requestOptions token')

            resp' <- extractRight resp

            fromResult . fromJSON $ resp'

          withRequestLog env = env { formatter = requestFormatter }

          requestFormatter :: Level -> FormattedTime -> Text -> Text -> LogStr
          requestFormatter level time env msg = toLogStr ("[Request Route: "+|toRoute r ^. #path|+"]" :: Text)
                                                <> defaultFormatter level time env msg

defaultRequestOptions :: Options
defaultRequestOptions =
  defaults
    & header "User-Agent" .~ [ "YAHDL (https://github.com/nitros12/yet-another-haskell-discord-library)" ]
    & checkResponse
    ?~ (\_ _ -> pure ())


requestOptions :: Token -> Options
requestOptions t =
  defaultRequestOptions & header "Authorization" .~ [formatToken t]
