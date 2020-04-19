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
import           Calamity.Types.Token

import           Data.Aeson                       hiding ( Options )
import qualified Data.ByteString.Lazy             as LB
import           Data.String                      ( String )
import           Data.Text.Strict.Lens

import           Network.Wreq
import           Network.Wreq.Types               ( Patchable, Postable, Putable )

import           Polysemy                         ( Sem )
import qualified Polysemy                         as P
import qualified Polysemy.Error                   as P
import qualified Polysemy.Reader                  as P

fromResult :: P.Member (P.Error RestError) r => Result a -> Sem r a
fromResult (Success a) = pure a
fromResult (Error e) = P.throw (DecodeError $ e ^. packed)

fromJSONDecode :: P.Member (P.Error RestError) r => Either String a -> Sem r a
fromJSONDecode (Right a) = pure a
fromJSONDecode (Left e) = P.throw (DecodeError $ e ^. packed)

extractRight :: P.Member (P.Error e) r => Either e a -> Sem r a
extractRight (Left e) = P.throw e
extractRight (Right a) = pure a

class ReadResponse a where
  readResp :: LB.ByteString -> Either String a

instance ReadResponse () where
  readResp = const (Right ())

instance {-# OVERLAPS #-}FromJSON a => ReadResponse a where
  readResp = eitherDecode

class Request a r | a -> r where
  toRoute :: a -> Route

  url :: a -> String
  url r = path (toRoute r) ^. unpacked

  toAction :: a -> Options -> String -> IO (Response LB.ByteString)

  invokeRequest :: forall reffs. (BotC reffs, FromJSON r) => a -> Sem reffs (Either RestError r)
  invokeRequest a = do
      rlState' <- P.asks rlState
      token' <- P.asks token

      resp <- attr "route" (toRoute a ^. #path) $ doRequest rlState' (toRoute a)
        (toAction a (requestOptions token') (Calamity.HTTP.Internal.Request.url a))

      P.runError $ (fromResult . fromJSON) =<< (fromJSONDecode . readResp) =<< extractRight resp

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

patchWith' :: Patchable a => a -> Options -> String -> IO (Response LB.ByteString)
patchWith' p o s = patchWith o s p

putEmpty :: Options -> String -> IO (Response LB.ByteString)
putEmpty o s = putWith o s ("" :: ByteString)

getWithP :: (Options -> Options) -> Options -> String -> IO (Response LB.ByteString)
getWithP oF o = getWith (oF o)
