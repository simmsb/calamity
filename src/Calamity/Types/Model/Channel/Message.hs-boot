-- | A message from a channel
module Calamity.Types.Model.Channel.Message
    ( Message
    , MessageType ) where

data Message

instance Show Message
instance Eq Message

data MessageType

instance Show MessageType
instance Eq MessageType
