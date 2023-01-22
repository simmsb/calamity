-- | Things that can be fetched from the discord CDN
module Calamity.Types.CDNAsset (
  CDNAsset (..),
  fetchAsset,
  fetchAsset',
)
where

import Control.Exception.Safe qualified as Ex
import Data.ByteString.Lazy (ByteString)
import Network.HTTP.Req qualified as Req
import Polysemy qualified as P

-- | Retrieve the asset from the CDN, like 'fetchAsset' but gives you more control
fetchAsset' :: (CDNAsset a, Req.MonadHttp m) => a -> m ByteString
fetchAsset' a = Req.responseBody <$> Req.req Req.GET (assetURL a) Req.NoReqBody Req.lbsResponse mempty

-- | Retrieve the asset from the CDN
fetchAsset :: (CDNAsset a, P.Member (P.Embed IO) r) => a -> P.Sem r (Either Req.HttpException ByteString)
fetchAsset a = P.embed $ Ex.catch (Right <$> r) (\(e :: Req.HttpException) -> pure $ Left e)
  where
    r = Req.runReq reqConfig $ fetchAsset' a

reqConfig :: Req.HttpConfig
reqConfig =
  Req.defaultHttpConfig
    { Req.httpConfigCheckResponse = \_ _ _ -> Nothing
    }

class CDNAsset a where
  assetURL :: a -> Req.Url 'Req.Https
