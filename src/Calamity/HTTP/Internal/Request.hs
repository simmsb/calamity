-- | Generic Request type
module Calamity.HTTP.Internal.Request
    ( Request(..)
    , postWith'
    , postWithP'
    , putWith'
    , patchWith'
    , putEmpty
    , putEmptyP
    , postEmpty
    , postEmptyP
    , getWithP ) where

import           Calamity.Client.Types
import           Calamity.HTTP.Internal.Ratelimit
import           Calamity.HTTP.Internal.Route
import           Calamity.HTTP.Internal.Types
import           Calamity.Metrics.Eff
import           Calamity.Types.Token

import           Control.Lens
import           Control.Monad

import           Data.Aeson                       hiding ( Options )
import           Data.ByteString                  ( ByteString )
import qualified Data.ByteString.Lazy             as LB
import qualified Data.Text.Encoding               as TS
import qualified Data.Text.Lazy                   as TL
import           Data.Text.Strict.Lens

import           DiPolysemy                       hiding ( debug, error, info )

import           Network.Wreq
import           Network.Wreq.Types               ( Patchable, Postable, Putable )

import           Polysemy                         ( Sem )
import qualified Polysemy                         as P
import qualified Polysemy.Error                   as P
import qualified Polysemy.Reader                  as P

fromResult :: P.Member (P.Error RestError) r => Data.Aeson.Result a -> Sem r a
fromResult (Success a) = pure a
fromResult (Error e) = P.throw (DecodeError . TL.pack $ e)

fromJSONDecode :: P.Member (P.Error RestError) r => Either String a -> Sem r a
fromJSONDecode (Right a) = pure a
fromJSONDecode (Left e) = P.throw (DecodeError . TL.pack $ e)

extractRight :: P.Member (P.Error e) r => Either e a -> Sem r a
extractRight (Left e) = P.throw e
extractRight (Right a) = pure a

class ReadResponse a where
  readResp :: LB.ByteString -> Either String a

instance ReadResponse () where
  readResp = const (Right ())

instance {-# OVERLAPS #-}FromJSON a => ReadResponse a where
  readResp = eitherDecode

class Request a where
  type Result a

  route :: a -> Route

  action :: a -> Options -> String -> IO (Response LB.ByteString)

  invoke :: (BotC r, FromJSON (Calamity.HTTP.Internal.Request.Result a)) => a -> Sem r (Either RestError (Calamity.HTTP.Internal.Request.Result a))
  invoke a = do
      rlState' <- P.asks rlState
      token' <- P.asks token

      let route' = route a

      inFlightRequests <- registerGauge "inflight_requests" [("route", route' ^. #path)]
      totalRequests <- registerCounter "total_requests" [("route", route' ^. #path)]
      void $ modifyGauge succ inFlightRequests
      void $ addCounter 1 totalRequests

      resp <- attr "route" (route' ^. #path) $ doRequest rlState' route'
        (action a (requestOptions token') (route' ^. #path . unpacked))

      void $ modifyGauge pred inFlightRequests

      P.runError $ (fromResult . fromJSON) =<< (fromJSONDecode . readResp) =<< extractRight resp

defaultRequestOptions :: Options
defaultRequestOptions = defaults
  & header "User-Agent" .~ ["Calamity (https://github.com/nitros12/calamity)"]
  & header "X-RateLimit-Precision" .~ ["millisecond"]
  & checkResponse ?~ (\_ _ -> pure ())

requestOptions :: Token -> Options
requestOptions t = defaultRequestOptions
  & header "Authorization" .~ [TS.encodeUtf8 . TL.toStrict $ formatToken t]

postWith' :: Postable a => a -> Options -> String -> IO (Response LB.ByteString)
postWith' p o s = postWith o s p

postWithP' :: Postable a => a -> (Options -> Options) -> Options -> String -> IO (Response LB.ByteString)
postWithP' p oF o s = postWith (oF o) s p

postEmpty :: Options -> String -> IO (Response LB.ByteString)
postEmpty o s = postWith o s ("" :: ByteString)

putWith' :: Putable a => a -> Options -> String -> IO (Response LB.ByteString)
putWith' p o s = putWith o s p

patchWith' :: Patchable a => a -> Options -> String -> IO (Response LB.ByteString)
patchWith' p o s = patchWith o s p

putEmpty :: Options -> String -> IO (Response LB.ByteString)
putEmpty o s = putWith o s ("" :: ByteString)

putEmptyP :: (Options -> Options) -> Options -> String -> IO (Response LB.ByteString)
putEmptyP = (putEmpty .)

postEmptyP :: (Options -> Options) -> Options -> String -> IO (Response LB.ByteString)
postEmptyP = (postEmpty .)

getWithP :: (Options -> Options) -> Options -> String -> IO (Response LB.ByteString)
getWithP oF o = getWith (oF o)
