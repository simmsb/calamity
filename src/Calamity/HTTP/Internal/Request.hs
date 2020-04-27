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
import           Calamity.Internal.Utils
import           Calamity.Metrics.Eff
import           Calamity.Types.Token

import           Control.Lens
import           Control.Monad

import           Data.Aeson                       hiding ( Options )
import           Data.ByteString                  ( ByteString )
import qualified Data.ByteString.Lazy             as LB
import qualified Data.Text                        as TS
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

import           TextShow

fromResult :: P.Member (P.Error RestError) r => Result a -> Sem r a
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

class Request a r | a -> r where
  toRoute :: a -> Route

  url :: a -> String
  url = TS.unpack . path . toRoute

  toAction :: a -> Options -> String -> IO (Response LB.ByteString)

  invokeRequest :: forall reffs. (BotC reffs, FromJSON r) => a -> Sem reffs (Either RestError r)
  invokeRequest a = do
      rlState' <- P.asks rlState
      token' <- P.asks token

      let route = toRoute a

      inFlightRequests <- registerGauge "inflight_requests" [("route", route ^. #path)]
      totalRequests <- registerCounter "total_requests" [("route", route ^. #path)]
      void $ modifyGauge succ inFlightRequests
      void $ addCounter 1 totalRequests

      whenJust (route ^. #guildID) $ \guildID -> do
        totalRequestsGuild <- registerCounter "total_requests" [("guild", showt guildID)]
        void $ addCounter 1 totalRequestsGuild

      resp <- attr "route" (toRoute a ^. #path) $ doRequest rlState' route
        (toAction a (requestOptions token') (route ^. #path . unpacked))

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
