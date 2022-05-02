{-# LANGUAGE TemplateHaskell #-}

-- | Emoji endpoints
module Calamity.HTTP.Emoji (
  EmojiRequest (..),
  CreateGuildEmojiOptions (..),
  ModifyGuildEmojiOptions (..),
) where

import Calamity.HTTP.Internal.Request
import Calamity.HTTP.Internal.Route
import Calamity.Internal.Utils (CalamityToJSON (..), CalamityToJSON' (..), (.=))
import Calamity.Types.Model.Guild
import Calamity.Types.Snowflake
import qualified Data.Aeson as Aeson
import Data.Function
import Data.Text (Text)
import Network.HTTP.Req
import Optics.TH

data CreateGuildEmojiOptions = CreateGuildEmojiOptions
  { name :: Text
  , image :: Text
  , roles :: [Snowflake Role]
  }
  deriving (Show)
  deriving (Aeson.ToJSON) via CalamityToJSON CreateGuildEmojiOptions

instance CalamityToJSON' CreateGuildEmojiOptions where
  toPairs CreateGuildEmojiOptions {..} =
    [ "name" .= name
    , "image" .= image
    , "roles" .= roles
    ]

data ModifyGuildEmojiOptions = ModifyGuildEmojiOptions
  { name :: Text
  , roles :: [Snowflake Role]
  }
  deriving (Show)
  deriving (Aeson.ToJSON) via CalamityToJSON ModifyGuildEmojiOptions

instance CalamityToJSON' ModifyGuildEmojiOptions where
  toPairs ModifyGuildEmojiOptions {..} =
    [ "name" .= name
    , "roles" .= roles
    ]

data EmojiRequest a where
  ListGuildEmojis :: (HasID Guild g) => g -> EmojiRequest [Emoji]
  GetGuildEmoji :: (HasID Guild g, HasID Emoji e) => g -> e -> EmojiRequest Emoji
  CreateGuildEmoji :: (HasID Guild g) => g -> CreateGuildEmojiOptions -> EmojiRequest Emoji
  ModifyGuildEmoji :: (HasID Guild g, HasID Emoji e) => g -> e -> ModifyGuildEmojiOptions -> EmojiRequest Emoji
  DeleteGuildEmoji :: (HasID Guild g, HasID Emoji e) => g -> e -> EmojiRequest ()

baseRoute :: Snowflake Guild -> RouteBuilder _
baseRoute id = mkRouteBuilder // S "guilds" // ID @Guild // S "emojis" & giveID id

instance Request (EmojiRequest a) where
  type Result (EmojiRequest a) = a

  route (ListGuildEmojis (getID -> gid)) = baseRoute gid & buildRoute
  route (GetGuildEmoji (getID -> gid) (getID @Emoji -> eid)) =
    baseRoute gid // ID @Emoji
      & giveID eid
      & buildRoute
  route (CreateGuildEmoji (getID -> gid) _) = baseRoute gid & buildRoute
  route (ModifyGuildEmoji (getID -> gid) (getID @Emoji -> eid) _) =
    baseRoute gid // ID @Emoji
      & giveID eid
      & buildRoute
  route (DeleteGuildEmoji (getID -> gid) (getID @Emoji -> eid)) =
    baseRoute gid // ID @Emoji
      & giveID eid
      & buildRoute

  action (ListGuildEmojis _) = getWith
  action (GetGuildEmoji _ _) = getWith
  action (CreateGuildEmoji _ o) = postWith' (ReqBodyJson o)
  action (ModifyGuildEmoji _ _ o) = patchWith' (ReqBodyJson o)
  action (DeleteGuildEmoji _ _) = deleteWith

$(makeFieldLabelsNoPrefix ''CreateGuildEmojiOptions)
$(makeFieldLabelsNoPrefix ''ModifyGuildEmojiOptions)
