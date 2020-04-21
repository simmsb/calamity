{-# LANGUAGE TemplateHaskell #-}

-- | Effect for handling the cache
module Calamity.Cache.Eff
    ( CacheEff(..)
    , setBotUser
    , updateBotUser
    , getBotUser
    , setGuild
    , updateGuild
    , getGuild
    , delGuild
    , setDM
    , updateDM
    , getDM
    , delDM
      -- , setGuildChannel
      -- , getGuildChannel
      -- , delGuildChannel
    , setUser
    , updateUser
    , getUser
    , delUser
    , setUnavailableGuild
    , isUnavailableGuild
    , delUnavailableGuild
    , setMessage
    , updateMessage
    , getMessage
    , delMessage ) where

import           Calamity.Types.Model.Channel
import           Calamity.Types.Model.Guild
import           Calamity.Types.Model.User
import           Calamity.Types.Snowflake

import           Polysemy
import qualified Polysemy as P

data CacheEff m a where
  -- | Set the 'User' representing the bot itself
  SetBotUser :: User -> CacheEff m ()
  -- | Get the 'User' representing the bot itself
  GetBotUser :: CacheEff m (Maybe User)

  -- | Set or Update a 'Guild' in the cache
  SetGuild :: Guild -> CacheEff m ()
  -- | Get a 'Guild' from the cache
  GetGuild :: Snowflake Guild -> CacheEff m (Maybe Guild)
  -- | Delete a 'Guild' from the cache
  DelGuild :: Snowflake Guild -> CacheEff m ()

  -- | Set or Update a 'DMChannel' in the cache
  SetDM :: DMChannel -> CacheEff m ()
  -- | Get a 'DMChannel' from the cache
  GetDM :: Snowflake DMChannel -> CacheEff m (Maybe DMChannel)
  -- | Delete a 'DMChannel' from the cache
  DelDM :: Snowflake DMChannel -> CacheEff m ()

  -- -- | Set or Update a 'GuildChannel' in the cache
  -- SetGuildChannel :: GuildChannel -> CacheEff m ()
  -- -- | Get a 'GuildChannel' from the cache
  -- GetGuildChannel :: Snowflake GuildChannel -> CacheEff m (Maybe GuildChannel)
  -- -- | Delete a 'GuildChannel' from the cache
  -- DelGuildChannel :: Snowflake GuildChannel -> CacheEff m ()
  -- | Set or Update a 'User' in the cache
  SetUser :: User -> CacheEff m ()
  -- | Get a 'User' from the cache
  GetUser :: Snowflake User -> CacheEff m (Maybe User)
  -- | Delete a 'User' from the cache
  DelUser :: Snowflake User -> CacheEff m ()

  -- | Flag a 'Guild' as unavailable
  SetUnavailableGuild :: Snowflake Guild -> CacheEff m ()
  -- | Test if a 'Guild' is flagged as unavailable
  IsUnavailableGuild :: Snowflake Guild -> CacheEff m Bool
  -- | Unflag a 'Guild' from being unavailable
  DelUnavailableGuild :: Snowflake Guild -> CacheEff m ()

  -- | Add or Update a 'Message' in the cache
  SetMessage :: Message -> CacheEff m ()
  -- | Get a 'Message' from the cache
  GetMessage :: Snowflake Message -> CacheEff m (Maybe Message)
  -- | Delete a 'Message' from the cache
  DelMessage :: Snowflake Message -> CacheEff m ()

makeSem ''CacheEff

updateBotUser :: P.Member CacheEff r => (User -> User) -> Sem r ()
updateBotUser f = getBotUser >>= flip whenJust (setBotUser . f)

updateGuild :: P.Member CacheEff r => Snowflake Guild -> (Guild -> Guild) -> Sem r ()
updateGuild id f = getGuild id >>= flip whenJust (setGuild . f)

updateDM :: P.Member CacheEff r => Snowflake DMChannel -> (DMChannel -> DMChannel) -> Sem r ()
updateDM id f = getDM id >>= flip whenJust (setDM . f)

updateUser :: P.Member CacheEff r => Snowflake User -> (User -> User) -> Sem r ()
updateUser id f = getUser id >>= flip whenJust (setUser . f)

updateMessage :: P.Member CacheEff r => Snowflake Message -> (Message -> Message) -> Sem r ()
updateMessage id f = getMessage id >>= flip whenJust (setMessage . f)
