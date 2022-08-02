{-# LANGUAGE TemplateHaskell #-}

module Calamity.Types.Model.Avatar (
  Avatar (..),
  MemberAvatar (..),
) where

import Calamity.Types.CDNAsset (CDNAsset (..))
import {-# SOURCE #-} Calamity.Types.Model.Guild.Guild
import {-# SOURCE #-} Calamity.Types.Model.User
import Calamity.Types.Snowflake (Snowflake, fromSnowflake)
import Calamity.Utils.CDNUrl (assetHashFile, cdnURL)
import qualified Data.Text as T
import Network.HTTP.Req ((/:), (/~))
import Optics (makeFieldLabelsNoPrefix)
import TextShow (showt)
import TextShow.TH (deriveTextShow)

data Avatar = Avatar
  { hash :: Maybe T.Text
  , userID :: Snowflake User
  , discrim :: Int
  }
  deriving (Show, Eq)

instance CDNAsset Avatar where
  assetURL Avatar {hash = Just h, userID} =
    cdnURL /: "avatars" /~ fromSnowflake userID /: assetHashFile h
  assetURL Avatar {discrim} =
    cdnURL /: "embed" /: "avatars" /: (showt (discrim `mod` 5) <> ".png")

-- | A member's custom guild avatar
data MemberAvatar = MemberAvatar
  { hash :: T.Text
  , guildID :: Snowflake Guild
  , userID :: Snowflake User
  }
  deriving (Show, Eq)

instance CDNAsset MemberAvatar where
  assetURL MemberAvatar {hash, guildID, userID} =
    cdnURL /: "guilds" /~ guildID /: "users" /~ userID /: "avatars" /: assetHashFile hash

$(deriveTextShow ''Avatar)
$(makeFieldLabelsNoPrefix ''Avatar)
$(deriveTextShow ''MemberAvatar)
$(makeFieldLabelsNoPrefix ''MemberAvatar)
