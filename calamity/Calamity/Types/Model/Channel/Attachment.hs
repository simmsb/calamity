{-# LANGUAGE TemplateHaskell #-}

-- | Message attachments
module Calamity.Types.Model.Channel.Attachment (
  Attachment (..),
) where

import Calamity.Types.Snowflake
import Data.Aeson ((.:), (.:?))
import Data.Aeson qualified as Aeson
import Data.Text (Text)
import Data.Word
import Optics.TH
import TextShow.TH

fuseTup2 :: Monad f => (f a, f b) -> f (a, b)
fuseTup2 (a, b) = do
  !a' <- a
  !b' <- b
  pure (a', b')

data Attachment = Attachment
  { id :: Snowflake Attachment
  , filename :: Text
  , size :: Word64
  , url :: Text
  , proxyUrl :: Text
  , dimensions :: Maybe (Word64, Word64)
  }
  deriving (Eq, Show)
  deriving (HasID Attachment) via HasIDField "id" Attachment

$(deriveTextShow ''Attachment)
$(makeFieldLabelsNoPrefix ''Attachment)

instance Aeson.FromJSON Attachment where
  parseJSON = Aeson.withObject "Attachment" $ \v -> do
    width <- v .:? "width"
    height <- v .:? "height"

    Attachment
      <$> v .: "id"
      <*> v .: "filename"
      <*> v .: "size"
      <*> v .: "url"
      <*> v .: "proxy_url"
      <*> pure
        (fuseTup2 (width, height))
