-- | Permission utilities
module Calamity.Utils.Permissions
    ( PermissionsIn(..)
    , PermissionsIn'(..)
    , basePermissions
    , applyOverwrites ) where

import           Calamity.Client.Types
import           Calamity.Types.Model.Channel.Guild
import           Calamity.Types.Model.Guild.Guild
import           Calamity.Types.Model.Guild.Member
import           Calamity.Types.Model.Guild.Overwrite
import           Calamity.Types.Model.Guild.Permissions
import           Calamity.Types.Snowflake
import           Calamity.Types.Upgradeable

import           Control.Lens

import           Data.Flags
import qualified Data.Vector.Unboxed                    as V

import qualified Polysemy                               as P

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

class PermissionsIn a where
  permissionsIn :: a -> Member -> Permissions

instance PermissionsIn (Guild, GuildChannel) where
  permissionsIn (g, c) m = applyOverwrites c m $ basePermissions g m

instance PermissionsIn Guild where
  permissionsIn g m = basePermissions g m

class PermissionsIn' a where
  permissionsIn' :: BotC r => a -> Member -> P.Sem r Permissions

instance PermissionsIn' GuildChannel where
  permissionsIn' c m = do
    g <- upgrade (getID @Guild c)
    case g of
      Just g' -> pure $ permissionsIn (g', c) m
      Nothing -> pure noFlags

instance PermissionsIn' Guild where
  permissionsIn' g m = pure $ permissionsIn g m

instance PermissionsIn' (Snowflake GuildChannel) where
  permissionsIn' cid m = do
    c <- upgrade cid
    case c of
      Just c' -> permissionsIn' c' m
      Nothing -> pure noFlags

instance PermissionsIn' (Snowflake Guild) where
  permissionsIn' gid m = do
    g <- upgrade gid
    case g of
      Just g' -> permissionsIn' g' m
      Nothing -> pure noFlags
