-- | Updateable objects
module Calamity.Types.Updateable
    ( Updateable(..) ) where

import           Calamity.Types.General

import           Data.Generics.Product.Fields

class Updateable a where
  type Updated a

  update :: Updated a -> a -> a

setF :: forall (f :: Symbol) a b o n. (HasField f o o a b, HasField f o o b b, HasField f n n b b) => n -> o -> o
setF n = field @f .~ (n ^. field @f)

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
    & setF @"editedTimestamp" n
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
    & setF @"name" n
    & setF @"icon" n
    & setF @"splash" n
    & setF @"owner" n
    & setF @"ownerID" n
    & mergeF @"permissions" o n
    & setF @"region" n
    & setF @"afkChannelID" n
    & setF @"afkTimeout" n
    & mergeF @"embedEnabled" o n
    & setF @"embedChannelID" n
    & setF @"verificationLevel" n
    & setF @"defaultMessageNotifications" n
    & setF @"explicitContentFilter" n
    & setF @"roles" n
    & setF @"features" n
    & setF @"mfaLevel" n
    & setF @"applicationID" n
    & mergeF @"widgetEnabled" o n
    & setF @"widgetChannelID" n
    & setF @"systemChannelID" n
