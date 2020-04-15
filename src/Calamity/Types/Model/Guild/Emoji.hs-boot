-- | Discord emojis
module Calamity.Types.Model.Guild.Emoji
    ( Emoji
    , RawEmoji ) where

data Emoji

instance Show Emoji
instance Eq Emoji

data RawEmoji

instance Show RawEmoji
instance Eq RawEmoji
