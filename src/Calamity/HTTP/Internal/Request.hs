-- | Generic Request type
module Calamity.HTTP.Internal.Request
    ( Request(..)
    , postWith'
    , putWith'
    , getWithP ) where

import           Calamity.Client.Types
import           Calamity.HTTP.Internal.Ratelimit
import           Calamity.HTTP.Internal.Route
import           Calamity.HTTP.Internal.Types
import           Calamity.Types.General

import           Data.Aeson                       hiding ( Options )
import qualified Data.ByteString.Lazy             as LB
import           Data.String                      ( String )
import           Data.Text.Strict.Lens

import           Network.Wreq
import           Network.Wreq.Types               ( Postable, Putable )

fromResult :: Monad m => Result a -> ExceptT RestError m a
fromResult (Success a) = pure a
fromResult (Error e) = throwE (DecodeError $ e ^. packed)

extractRight :: Monad m => Either a b -> ExceptT a m b
extractRight (Left a) = throwE a
extractRight (Right a) = pure a

class Request a r | a -> r where
  toRoute :: a -> Route

  url :: a -> String
  url r = path (toRoute r) ^. unpacked

  toAction :: a -> Options -> String -> IO (Response LB.ByteString)

  -- TODO: instead of using BotM, instead use a generic HasRatelimits monad
  -- so that we can make requests from shards too
  invokeRequest :: FromJSON r => a -> EventM (Either RestError r)
  invokeRequest r = runExceptT inner
    where
      inner :: ExceptT RestError EventM r
      inner = do
        rlState' <- asks rlState
        token' <- asks token

        resp <- scope ("[Request Route: " +| toRoute r ^. #path |+ "]") $ doRequest rlState' (toRoute r)
          (toAction r (requestOptions token') (Calamity.HTTP.Internal.Request.url r))

        resp' <- extractRight resp

        fromResult . fromJSON $ resp'

defaultRequestOptions :: Options
defaultRequestOptions = defaults
  & header "User-Agent" .~ ["Calamity (https://github.com/nitros12/yet-another-haskell-discord-library)"]
  & checkResponse ?~ (\_ _ -> pure ())

requestOptions :: Token -> Options
requestOptions t = defaultRequestOptions
  & header "Authorization" .~ [encodeUtf8 $ formatToken t]

postWith' :: Postable a => a -> Options -> String -> IO (Response LB.ByteString)
postWith' p o s = postWith o s p

putWith' :: Putable a => a -> Options -> String -> IO (Response LB.ByteString)
putWith' p o s = putWith o s p

getWithP :: (Options -> Options) -> Options -> String -> IO (Response LB.ByteString)
getWithP oF o s = getWith (oF o) s
