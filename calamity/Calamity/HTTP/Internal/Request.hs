-- | Generic Request type
module Calamity.HTTP.Internal.Request (
  Request (..),
  invoke,
  getWith,
  postWith',
  postWithP',
  putWith',
  patchWith',
  putEmpty,
  putEmptyP,
  postEmpty,
  postEmptyP,
  getWithP,
  deleteWith,
  (=:?),
) where

import Calamity.Client.Types
import Calamity.HTTP.Internal.Ratelimit
import Calamity.HTTP.Internal.Route
import Calamity.HTTP.Internal.Types
import Calamity.Metrics.Eff
import Calamity.Types.Token

import Control.Lens
import Control.Monad

import Data.Aeson hiding (Options)
import Data.Aeson.Types (parseEither)
import qualified Data.ByteString.Lazy as LB
import qualified Data.Text as T
import qualified Data.Text.Encoding as TS

import DiPolysemy hiding (debug, error, info)

import Network.HTTP.Req
import Web.HttpApiData

import Polysemy (Sem)
import qualified Polysemy as P
import qualified Polysemy.Error as P
import qualified Polysemy.Reader as P

throwIfLeft :: P.Member (P.Error RestError) r => Either String a -> Sem r a
throwIfLeft (Right a) = pure a
throwIfLeft (Left e) = P.throw (InternalClientError . T.pack $ e)

extractRight :: P.Member (P.Error e) r => Either e a -> Sem r a
extractRight (Left e) = P.throw e
extractRight (Right a) = pure a

class ReadResponse a where
  processResp :: LB.ByteString -> (Value -> Value) -> Either String a

instance {-# OVERLAPPABLE #-} FromJSON a => ReadResponse a where
  processResp s f = eitherDecode s >>= parseEither parseJSON . f

instance ReadResponse () where
  processResp _ _ = Right ()

class Request a where
  type Result a

  route :: a -> Route

  action :: a -> Url 'Https -> Option 'Https -> Req LbsResponse

  modifyResponse :: a -> Value -> Value
  modifyResponse _ = id

invoke :: (BotC r, Request a, ReadResponse (Calamity.HTTP.Internal.Request.Result a)) => a -> Sem r (Either RestError (Calamity.HTTP.Internal.Request.Result a))
invoke a = do
  rlState' <- P.asks (^. #rlState)
  token' <- P.asks (^. #token)

  let route' = route a

  inFlightRequests <- registerGauge "inflight_requests" [("route", renderUrl $ route' ^. #path)]
  totalRequests <- registerCounter "total_requests" [("route", renderUrl $ route' ^. #path)]
  void $ modifyGauge (+ 1) inFlightRequests
  void $ addCounter 1 totalRequests

  let r = action a (route' ^. #path) (requestOptions token')
      act = runReq reqConfig r

  resp <- push "calamity" . attr "route" (renderUrl $ route' ^. #path) $ doRequest rlState' route' act

  void $ modifyGauge (subtract 1) inFlightRequests

  P.runError $ do
    s <- extractRight resp
    throwIfLeft $ processResp s (modifyResponse a)

reqConfig :: HttpConfig
reqConfig =
  defaultHttpConfig
    { httpConfigCheckResponse = \_ _ _ -> Nothing
    }

defaultRequestOptions :: Option 'Https
defaultRequestOptions =
  header "User-Agent" "Calamity (https://github.com/simmsb/calamity)"
    <> header "X-RateLimit-Precision" "millisecond"

requestOptions :: Token -> Option 'Https
requestOptions t = defaultRequestOptions <> header "Authorization" (TS.encodeUtf8 $ formatToken t)

getWith :: Url 'Https -> Option 'Https -> Req LbsResponse
getWith u = req GET u NoReqBody lbsResponse

postWith' :: HttpBody a => a -> Url 'Https -> Option 'Https -> Req LbsResponse
postWith' a u = req POST u a lbsResponse

postWithP' :: HttpBody a => a -> Option 'Https -> Url 'Https -> Option 'Https -> Req LbsResponse
postWithP' a o u o' = req POST u a lbsResponse (o <> o')

postEmpty :: Url 'Https -> Option 'Https -> Req LbsResponse
postEmpty u = req POST u NoReqBody lbsResponse

putWith' :: HttpBody a => a -> Url 'Https -> Option 'Https -> Req LbsResponse
putWith' a u = req PUT u a lbsResponse

patchWith' :: HttpBody a => a -> Url 'Https -> Option 'Https -> Req LbsResponse
patchWith' a u = req PATCH u a lbsResponse

putEmpty :: Url 'Https -> Option 'Https -> Req LbsResponse
putEmpty u = req PUT u NoReqBody lbsResponse

putEmptyP :: Option 'Https -> Url 'Https -> Option 'Https -> Req LbsResponse
putEmptyP o u o' = req PUT u NoReqBody lbsResponse (o <> o')

postEmptyP :: Option 'Https -> Url 'Https -> Option 'Https -> Req LbsResponse
postEmptyP o u o' = req POST u NoReqBody lbsResponse (o <> o')

getWithP :: Option 'Https -> Url 'Https -> Option 'Https -> Req LbsResponse
getWithP o u o' = req GET u NoReqBody lbsResponse (o <> o')

deleteWith :: Url 'Https -> Option 'Https -> Req LbsResponse
deleteWith u = req DELETE u NoReqBody lbsResponse

(=:?) :: ToHttpApiData a => T.Text -> Maybe a -> Option 'Https
n =:? (Just x) = n =: x
_ =:? Nothing = mempty
