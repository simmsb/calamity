{-# LANGUAGE TemplateHaskell #-}

-- | Message reactions
module Calamity.Types.Model.Channel.Reaction (Reaction (..)) where

import Calamity.Internal.Utils (CalamityToJSON (..), CalamityToJSON' (toPairs), (.=))
import Calamity.Types.Model.Guild.Emoji
import Data.Aeson ((.:))
import Data.Aeson qualified as Aeson
import Optics.TH
import TextShow.TH

data Reaction = Reaction
  { count :: Integer
  , me :: Bool
  , emoji :: RawEmoji
  }
  deriving (Eq, Show)
  deriving (Aeson.ToJSON) via CalamityToJSON Reaction

instance CalamityToJSON' Reaction where
  toPairs Reaction {..} =
    [ "count" .= count
    , "me" .= me
    , "emoj" .= emoji
    ]

instance Aeson.FromJSON Reaction where
  parseJSON = Aeson.withObject "Reaction" $ \v ->
    Reaction
      <$> v .: "count"
      <*> v .: "me"
      <*> v .: "emoji"

$(deriveTextShow ''Reaction)
$(makeFieldLabelsNoPrefix ''Reaction)
