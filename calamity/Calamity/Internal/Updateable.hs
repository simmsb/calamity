-- | Updateable objects
module Calamity.Internal.Updateable (Updateable (..)) where

import Calamity.Internal.Utils
import Calamity.Types.Model.Channel
import Calamity.Types.Model.Channel.UpdatedMessage
import Calamity.Types.Model.Guild.Guild
import Calamity.Types.Model.User
import Data.Maybe
import GHC.TypeLits
import Optics

class Updateable a where
  type Updated a
  type Updated a = a

  update :: Updated a -> a -> a

-- | sets original field to new field
setF ::
  forall (f :: Symbol) o n b k.
  ( k ~ A_Lens
  , LabelOptic' f k o b
  , LabelOptic' f k n b
  ) =>
  n ->
  o ->
  o
setF n = labelOptic @f .~ n ^. labelOptic @f

-- | sets original field to unwrapped new field if new field is not Nothing
mergeF ::
  forall (f :: Symbol) o n b k.
  ( k ~ A_Lens
  , LabelOptic' f k o b
  , LabelOptic' f k n (Maybe b)
  ) =>
  n ->
  o ->
  o
mergeF n = labelOptic @f %~ \oldv -> fromMaybe oldv (n ^. labelOptic @f)

-- | sets original field to new field if new field is not nothing
mergeF' ::
  forall (f :: Symbol) old new v k.
  ( k ~ A_Lens
  , LabelOptic' f k old (Maybe v)
  , LabelOptic' f k new (Maybe v)
  ) =>
  new ->
  old ->
  old
mergeF' new = labelOptic @f %~ \oldv -> lastMaybe oldv (new ^. labelOptic @f)

-- | sets original field to new field if new field was present
updateNullableDest ::
  forall (f :: Symbol) old new v k.
  ( k ~ A_Lens
  , LabelOptic' f k old (Maybe v)
  , LabelOptic' f k new (Maybe (MaybeNull v))
  ) =>
  new ->
  old ->
  old
updateNullableDest new = case new ^. labelOptic @f of
  Just (NotNull x) -> labelOptic @f ?~ x
  Just WasNull -> labelOptic @f .~ Nothing
  Nothing -> Prelude.id

-- NOTE: afaik only messages get partial updates
instance Updateable Message where
  type Updated Message = UpdatedMessage

  update n o =
    o
      & mergeF @"content" n
      & updateNullableDest @"editedTimestamp" n
      & mergeF @"tts" n
      & mergeF @"mentionEveryone" n
      & mergeF @"mentions" n
      & mergeF @"mentionRoles" n
      & mergeF @"mentionChannels" n
      & mergeF @"attachments" n
      & mergeF @"embeds" n
      & mergeF @"reactions" n
      & mergeF @"pinned" n
      & mergeF @"type_" n
      & updateNullableDest @"activity" n
      & updateNullableDest @"application" n
      & updateNullableDest @"messageReference" n
      & mergeF @"flags" n
      & updateNullableDest @"referencedMessage" n
      & updateNullableDest @"interaction" n
      & mergeF @"components" n

instance Updateable Channel where
  update n _ = n

instance Updateable DMChannel where
  update n _ = n

instance Updateable GuildChannel where
  update n _ = n

instance Updateable Guild where
  type Updated Guild = UpdatedGuild

  update n o =
    o
      & setF @"name" n
      & setF @"icon" n
      & setF @"splash" n
      & setF @"owner" n
      & setF @"ownerID" n
      & mergeF @"permissions" n
      & setF @"afkChannelID" n
      & setF @"afkTimeout" n
      & mergeF @"embedEnabled" n
      & setF @"embedChannelID" n
      & setF @"verificationLevel" n
      & setF @"defaultMessageNotifications" n
      & setF @"explicitContentFilter" n
      & setF @"roles" n
      & setF @"features" n
      & setF @"mfaLevel" n
      & setF @"applicationID" n
      & mergeF @"widgetEnabled" n
      & setF @"widgetChannelID" n
      & setF @"systemChannelID" n
      & setF @"preferredLocale" n

instance Updateable User where
  update n o =
    o
      & setF @"username" n
      & setF @"discriminator" n
      & mergeF' @"bot" n
      & mergeF' @"avatar" n
      & mergeF' @"mfaEnabled" n
      & mergeF' @"verified" n
      & mergeF' @"email" n
      & mergeF' @"flags" n
      & mergeF' @"premiumType" n
