-- | A User
module Calamity.Types.Model.User
    ( User
    , StatusType ) where

data User

instance Show User
instance Eq User

data StatusType

instance Show StatusType
instance Eq StatusType
