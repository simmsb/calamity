-- | Discord Emojis
module Calamity.Types.Model.Guild.Emoji
    ( RawEmoji
    ) where

import Data.Aeson

data RawEmoji

instance Show RawEmoji
instance Eq RawEmoji

instance FromJSON RawEmoji
