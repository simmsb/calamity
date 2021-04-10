-- | A message from a channel
module Calamity.Types.Model.Channel.Message
    ( Message
    , MessageType
    , MessageReference ) where

data Message

instance Show Message
instance Eq Message

data MessageType

instance Show MessageType
instance Eq MessageType

data MessageReference

instance Show MessageReference
instance Eq MessageReference
