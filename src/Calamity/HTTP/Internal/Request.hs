-- | Generic Request type
module Calamity.HTTP.Internal.Request
    ( Request(..)
    , postWith'
    , putWith'
    , patchWith'
    , putEmpty
    , postEmpty
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

import qualified Polysemy.Error                        as P
import qualified Polysemy.Reader                       as P
import Polysemy (Sem)

fromResult :: Monad m => Result a -> ExceptT RestError m a
fromResult (Success a) = pure a
fromResult (Error e) = throwE (DecodeError $ e ^. packed)

fromJSONDecode :: Monad m => Either String a -> ExceptT RestError m a
fromJSONDecode (Right a) = pure a
fromJSONDecode (Left e) = throwE (DecodeError $ e ^. packed)

extractRight :: Monad m => Either a b -> ExceptT a m b
extractRight (Left a) = throwE a
extractRight (Right a) = pure a

class ReadResponse a where
  readResp :: LB.ByteString -> Either String a

instance ReadResponse () where
  readResp = const (Right ())

instance {-# OVERLAPS #-} FromJSON a => ReadResponse a where
  readResp = eitherDecode

class Request a r | a -> r where
  toRoute :: a -> Route

  url :: a -> String
  url r = path (toRoute r) ^. unpacked

  toAction :: a -> Options -> String -> IO (Response LB.ByteString)

  invokeRequest :: forall reffs. (BotC reffs, FromJSON r) => a -> Sem reffs (Either RestError r)
  invokeRequest a = runError inner
    where
      inner :: Sem (P.Error RestError ': reffs) r
      inner = do
        rlState' <- P.asks rlState
        token' <- P.asks token

        resp <- attr "route" (toRoute r ^. #path) $ doRequest rlState' (toRoute a)
          (toAction r (requestOptions token') (Calamity.HTTP.Internal.Request.url r))

        (fromResult . fromJSON) =<< (fromJSONDecode . readResp) =<< extractRight resp

defaultRequestOptions :: Options
defaultRequestOptions = defaults
  & header "User-Agent" .~ ["Calamity (https://github.com/nitros12/yet-another-haskell-discord-library)"]
  & header "X-RateLimit-Precision" .~ ["millisecond"]
  & checkResponse ?~ (\_ _ -> pure ())

requestOptions :: Token -> Options
requestOptions t = defaultRequestOptions
  & header "Authorization" .~ [encodeUtf8 $ formatToken t]

postWith' :: Postable a => a -> Options -> String -> IO (Response LB.ByteString)
postWith' p o s = postWith o s p

postEmpty :: Options -> String -> IO (Response LB.ByteString)
postEmpty o s = postWith o s ("" :: ByteString)

putWith' :: Putable a => a -> Options -> String -> IO (Response LB.ByteString)
putWith' p o s = putWith o s p

patchWith' :: Postable a => a -> Options -> String -> IO (Response LB.ByteString)
patchWith' p o s = patchWith o s p

putEmpty :: Options -> String -> IO (Response LB.ByteString)
putEmpty o s = putWith o s ("" :: ByteString)

getWithP :: (Options -> Options) -> Options -> String -> IO (Response LB.ByteString)
getWithP oF o = getWith (oF o)
