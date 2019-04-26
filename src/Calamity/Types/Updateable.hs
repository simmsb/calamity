-- | Updateable objects
module Calamity.Types.Updateable
    ( Updateable(..) ) where

import           Calamity.Types.General

import           Data.Generics.Product.Fields

class Updateable a where
  type Updated a

  update :: Updated a -> a -> a

setF :: forall (f :: Symbol) a b o n. (HasField f o o a b, HasField f o o b b, HasField f n n b b) => o -> n -> o -> o
setF o n = field @f .~ (n ^. field @f)

mergeF :: forall (f :: Symbol) a b o n.
       (HasField f o o a b, HasField f o o b b, HasField f n n (Maybe b) (Maybe b))
       => o
       -> n
       -> o
       -> o
mergeF o n = field @f .~ fromMaybe (o ^. field @f) (n ^. field @f)

instance Updateable Message where
  type Updated Message = UpdatedMessage

  update n o = o
    & mergeF @"content" o n
    & setF @"editedTimestamp" o n
    & mergeF @"tts" o n
    & mergeF @"mentionEveryone" o n
    & mergeF @"mentions" o n
    & mergeF @"mentionRoles" o n
    & mergeF @"attachments" o n
    & mergeF @"embeds" o n
    & mergeF @"reactions" o n
    & mergeF @"pinned" o n

instance Updateable Channel where
  type Updated Channel = Channel

  update n _ = n

instance Updateable Guild where
  type Updated Guild = UpdatedGuild

  -- For updating guild we just put in the non-present data from
  update n o = o
    & setF @"name" o n
    & setF @"icon" o n
    & setF @"splash" o n
    & setF @"owner" o n
    & setF @"ownerID" o n
    & mergeF @"permissions" o n
    & setF @"region" o n
    & setF @"afkChannelID" o n
    & setF @"afkTimeout" o n
    & mergeF @"embedEnabled" o n
    & setF @"embedChannelID" o n
    & setF @"verificationLevel" o n
    & setF @"defaultMessageNotifications" o n
    & setF @"explicitContentFilter" o n
    & setF @"roles" o n
    & setF @"features" o n
    & setF @"mfaLevel" o n
    & setF @"applicationID" o n
    & mergeF @"widgetEnabled" o n
    & setF @"widgetChannelID" o n
    & setF @"systemChannelID" o n
