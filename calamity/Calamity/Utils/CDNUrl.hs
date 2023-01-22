-- | CDN url stuff
module Calamity.Utils.CDNUrl (
  cdnURL,
  assetHashFile,
  isGIFAsset,
) where

import Data.Text qualified as T
import Network.HTTP.Req qualified as Req

-- | The CDN URL
cdnURL :: Req.Url 'Req.Https
cdnURL = Req.https "cdn.discordapp.com"

-- | Test if an asset hash is animated
isGIFAsset :: T.Text -> Bool
isGIFAsset = T.isPrefixOf "a_"

{- | Generate \'hash.ext\' for an asset hash. @ext@ will be \'gif\' for animated
 assets, \'png\' otherwise.
-}
assetHashFile :: T.Text -> T.Text
assetHashFile h = if isGIFAsset h then h <> ".gif" else h <> ".png"
