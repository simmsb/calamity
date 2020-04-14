-- | A message from a channel
module Calamity.Types.Model.Channel.Message
    ( Message
    , UpdatedMessage
    , MessageType ) where

data Message

instance Show Message
instance Eq Message

data UpdatedMessage

instance Show UpdatedMessage
instance Eq UpdatedMessage

data MessageType

instance Show MessageType
instance Eq MessageType
