-- | Permission utilities
module Calamity.Utils.Permissions
    ( basePermissions
    , applyOverwrites
    , PermissionsIn(..)
    , PermissionsIn'(..) ) where

import           Calamity.Client.Types
import           Calamity.Types.Model.Channel.Guild
import           Calamity.Types.Model.Guild.Guild
import           Calamity.Types.Model.Guild.Member
import           Calamity.Types.Model.Guild.Overwrite
import           Calamity.Types.Model.Guild.Permissions
import           Calamity.Types.Model.User
import           Calamity.Types.Snowflake
import           Calamity.Types.Upgradeable

import           Control.Lens

import           Data.Flags
import qualified Data.Vector.Unboxed                    as V

import qualified Polysemy                               as P

-- | Calculate a 'Member''s 'Permissions' in a 'Guild'
basePermissions :: Guild -> Member -> Permissions
basePermissions g m
  | (g ^. #ownerID == getID m) = allFlags
  | otherwise = let everyoneRole  = g ^. #roles . at (coerceSnowflake $ getID @Guild g)
                    permsEveryone = maybe noFlags (^. #permissions) everyoneRole
                    rolePerms     = g ^.. #roles . foldMap ix (V.toList $ m ^. #roles) . #permissions
                    perms         = foldl andFlags noFlags (permsEveryone:rolePerms)
                in if perms .<=. administrator
                   then allFlags
                   else perms

-- | Apply any 'Overwrite's for a 'GuildChannel' onto some 'Permissions'
applyOverwrites :: GuildChannel -> Member -> Permissions -> Permissions
applyOverwrites c m p
  | p .<=. administrator = allFlags
  | otherwise =
    let everyoneOverwrite = c ^. #permissionOverwrites . at (coerceSnowflake $ getID @Guild c)
        everyoneAllow     = maybe noFlags (^. #allow) everyoneOverwrite
        everyoneDeny      = maybe noFlags (^. #deny) everyoneOverwrite
        p'                = p .-. everyoneDeny .+. everyoneAllow
        roleOverwrites    = c ^.. #permissionOverwrites . foldMap ix
          (map (coerceSnowflake @_ @Overwrite) . V.toList $ m ^. #roles)
        roleAllow         = foldl andFlags noFlags (roleOverwrites ^.. traverse . #allow)
        roleDeny          = foldl andFlags noFlags (roleOverwrites ^.. traverse . #deny)
        p''               = p' .-. roleDeny .+. roleAllow
        memberOverwrite   = c ^. #permissionOverwrites . at (coerceSnowflake @_ @Overwrite $ getID @Member m)
        memberAllow       = maybe noFlags (^. #allow) memberOverwrite
        memberDeny        = maybe noFlags (^. #deny) memberOverwrite
        p'''              = p'' .-. memberDeny .+. memberAllow
    in p'''

-- | Things that 'Member's have 'Permissions' in
class PermissionsIn a where
  -- | Calculate a 'Member''s 'Permissions' in something
  permissionsIn :: a -> Member -> Permissions

-- | A 'Member''s 'Permissions' in a channel are their roles and overwrites
instance PermissionsIn (Guild, GuildChannel) where
  permissionsIn (g, c) m = applyOverwrites c m $ basePermissions g m

-- | A 'Member''s 'Permissions' in a guild are just their roles
instance PermissionsIn Guild where
  permissionsIn g m = basePermissions g m

-- | A variant of 'PermissionsIn' that will use the cache/http.
class PermissionsIn' a where
  -- | Calculate the permissions of something that has a 'User' id
  --
  -- If permissions could not be calculated because something couldn't be found
  -- in the cache, this will return an empty set of permissions. Use
  -- 'permissionsIn' if you want to handle cases where something might not exist
  -- in cache.
  permissionsIn' :: (BotC r, HasID User u) => a -> u -> P.Sem r Permissions

-- | A 'User''s 'Permissions' in a channel are their roles and overwrites
--
-- This will fetch the guild from the cache or http as needed
instance PermissionsIn' GuildChannel where
  permissionsIn' c (getID @User -> uid) = do
    m <- upgrade (getID @Guild c, coerceSnowflake @_ @Member uid)
    g <- upgrade (getID @Guild c)
    case (m, g) of
      (Just m, Just g') -> pure $ permissionsIn (g', c) m
      _                 -> pure noFlags

-- | A 'Member''s 'Permissions' in a guild are just their roles
instance PermissionsIn' Guild where
  permissionsIn' g (getID @User -> uid) = do
    m <- upgrade (getID @Guild g, coerceSnowflake @_ @Member uid)
    case m of
      Just m' -> pure $ permissionsIn g m'
      Nothing -> pure noFlags

-- | A 'Member''s 'Permissions' in a channel are their roles and overwrites
--
-- This will fetch the guild and channel from the cache or http as needed
instance PermissionsIn' (Snowflake GuildChannel) where
  permissionsIn' cid u = do
    c <- upgrade cid
    case c of
      Just c' -> permissionsIn' c' u
      _       -> pure noFlags

-- | A 'Member''s 'Permissions' in a guild are just their roles
--
-- This will fetch the guild from the cache or http as needed
instance PermissionsIn' (Snowflake Guild) where
  permissionsIn' gid u = do
    g <- upgrade gid
    case g of
      Just g' -> permissionsIn' g' u
      Nothing -> pure noFlags
