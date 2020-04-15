-- | Message attachments
module Calamity.Types.Model.Channel.Attachment
    ( Attachment(..) ) where

import           Calamity.Types.Snowflake

import           Data.Aeson

fuseTup2 :: Monad f => (f a, f b) -> f (a, b)
fuseTup2 (a, b) = do
  a' <- a
  b' <- b
  pure (a', b')

data Attachment = Attachment
  { id         :: Snowflake Attachment
  , filename   :: ShortText
  , size       :: Word64
  , url        :: ShortText
  , proxyUrl   :: ShortText
  , dimensions :: Maybe (Word64, Word64)
  }
  deriving ( Eq, Show, Generic )

instance ToJSON Attachment where
  toEncoding Attachment { id, filename, size, url, proxyUrl, dimensions = Just (width, height) } = pairs
    ("id" .= id <> "filename" .= filename <> "size" .= size <> "url" .= url <> "proxy_url" .= proxyUrl
     <> "width" .= width <> "height" .= height)

  toEncoding Attachment { id, filename, size, url, proxyUrl } = pairs
    ("id" .= id <> "filename" .= filename <> "size" .= size <> "url" .= url <> "proxy_url" .= proxyUrl)

instance FromJSON Attachment where
  parseJSON = withObject "Attachment" $ \v -> do
    width <- v .:? "width"
    height <- v .:? "height"

    Attachment <$> v .: "id" <*> v .: "filename" <*> v .: "size" <*> v .: "url" <*> v .: "proxy_url" <*> pure
      (fuseTup2 (width, height))
