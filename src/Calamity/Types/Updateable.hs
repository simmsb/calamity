-- | Updateable objects
module Calamity.Types.Updateable
    ( Updateable(..) ) where

import           Calamity.Types.General

import           Data.Generics.Product.Fields

class Updateable a where
  type UpdateType a

  update :: a -> UpdateType a -> a

instance Updateable Message where
  type UpdateType Message = UpdatedMessage

  update o n = o
    & updateF @"content"
    & field @"editedTimestamp" .~ (o ^. field @"editedTimestamp")
    & updateF @"tts"
    & updateF @"mentionEveryone"
    & updateF @"mentions"
    & updateF @"mentionRoles"
    & updateF @"attachments"
    & updateF @"embeds"
    & updateF @"reactions"
    & updateF @"pinned"
    where
      mergeF :: forall (f :: Symbol) a.
             (HasField f Message Message a a, HasField f UpdatedMessage UpdatedMessage (Maybe a) (Maybe a))
             => a
      mergeF = fromMaybe (o ^. field @f) (n ^. field @f)

      updateF :: forall (f :: Symbol) a b.
              ( HasField f Message Message a b
              , HasField f Message Message b b
              , HasField f UpdatedMessage UpdatedMessage (Maybe b) (Maybe b))
              => Message
              -> Message
      updateF = field @f .~ mergeF @f
