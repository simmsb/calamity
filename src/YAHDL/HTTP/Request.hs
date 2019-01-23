-- | Generic Request type

module YAHDL.HTTP.Request where

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

class Request a where
  type RespVal a

  toRoute :: a -> Route

  url :: a -> String
  url r = path (toRoute r) ^. unpacked

  toAction :: a -> IO (Response LB.ByteString)

  invokeRequest :: FromJSON a => a -> BotM (Either RestError a)
  invokeRequest r = runExceptT inner
    where inner :: ExceptT RestError BotM a
          inner = do
            rlState' <- asks rlState
            resp <- liftIO $ doRequest rlState' (toRoute r) (toAction r)

            resp' <- extractRight resp

            fromResult . fromJSON $ resp'

defaultRequestOptions :: Options
defaultRequestOptions =
  defaults & header "User-Agent" .~ ["YAHDL (https://github.com/nitros12/yet-another-haskell-discord-library)"]
           & checkResponse ?~ (\_ _ -> pure ())


requestOptions :: Token -> Options
requestOptions t =
  defaultRequestOptions & header "Authorization" .~ [formatToken t]
