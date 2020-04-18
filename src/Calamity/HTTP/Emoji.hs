-- | Emoji endpoints
module Calamity.HTTP.Emoji
    ( EmojiRequest(..) ) where

import           Calamity.HTTP.Internal.Request
import           Calamity.HTTP.Internal.Route
import           Calamity.Internal.AesonThings
import           Calamity.Types.Model.Guild
import           Calamity.Types.Snowflake

import           Data.Aeson

import           Network.Wreq


data CreateGuildEmojiOptions = CreateGuildEmojiOptions
  { name  :: ShortText
  , image :: ShortText
  , roles :: [Snowflake Role]
  }
  deriving ( Show, Generic )
  deriving ( ToJSON ) via CalamityJSON CreateGuildEmojiOptions

data ModifyGuildEmojiOptions = ModifyGuildEmojiOptions
  { name  :: ShortText
  , roles :: [Snowflake Role]
  }
  deriving ( Show, Generic )
  deriving ( ToJSON ) via CalamityJSON ModifyGuildEmojiOptions

data EmojiRequest a where
  ListGuildEmojis :: (HasID Guild g) => g -> EmojiRequest [Emoji]
  GetGuildEmoji :: (HasID Guild g, HasID Emoji e) => g -> e -> EmojiRequest Emoji
  CreateGuildEmoji :: (HasID Guild g) => g -> CreateGuildEmojiOptions -> EmojiRequest Emoji
  ModifyGuildEmoji :: (HasID Guild g, HasID Emoji e) => g -> e -> ModifyGuildEmojiOptions -> EmojiRequest Emoji
  DeleteGuildEmoji :: (HasID Guild g, HasID Emoji e) => g -> e -> EmojiRequest ()

baseRoute :: Snowflake Guild -> RouteBuilder _
baseRoute id = mkRouteBuilder // S "guilds" // ID @Guild // S "emojis" & giveID id

instance Request (EmojiRequest a) a where
  toRoute (ListGuildEmojis (getID -> gid)) = baseRoute gid & buildRoute
  toRoute (GetGuildEmoji (getID -> gid) (getID @Emoji -> eid)) = baseRoute gid // ID @Emoji
    & giveID eid
    & buildRoute
  toRoute (CreateGuildEmoji (getID -> gid) _) = baseRoute gid & buildRoute
  toRoute (ModifyGuildEmoji (getID -> gid) (getID @Emoji -> eid) _) = baseRoute gid // ID @Emoji
    & giveID eid
    & buildRoute
  toRoute (DeleteGuildEmoji (getID -> gid) (getID @Emoji -> eid)) = baseRoute gid // ID @Emoji
    & giveID eid
    & buildRoute

  toAction (ListGuildEmojis _)       = getWith
  toAction (GetGuildEmoji _ _)       = getWith
  toAction (CreateGuildEmoji _ o)    = postWith' (toJSON o)
  toAction (ModifyGuildEmoji _ _ o)  = patchWith' (toJSON o)
  toAction (DeleteGuildEmoji _ _)    = deleteWith
