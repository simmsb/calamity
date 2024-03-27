{-# LANGUAGE TemplateHaskell #-}

-- | Effect for handling the cache
module Calamity.Cache.Eff (
  CacheEff (..),
  setBotUser,
  updateBotUser,
  getBotUser,
  setGuild,
  updateGuild,
  getGuild,
  getGuildChannel,
  getGuilds,
  delGuild,
  setDM,
  updateDM,
  getDM,
  getDMs,
  delDM,
  -- , setGuildChannel
  -- , getGuildChannel
  -- , delGuildChannel
  setUser,
  updateUser,
  getUser,
  getUsers,
  delUser,
  setUnavailableGuild,
  isUnavailableGuild,
  getUnavailableGuilds,
  delUnavailableGuild,
  setMessage,
  updateMessage,
  getMessage,
  getMessages,
  delMessage,
) where

import Calamity.Internal.Utils
import Calamity.Types.Model.Channel
import Calamity.Types.Model.Guild
import Calamity.Types.Model.User
import Calamity.Types.Snowflake

import Polysemy
import Polysemy qualified as P

data CacheEff m a where
  -- | Set the 'User' representing the bot itself
  SetBotUser :: User -> CacheEff m ()
  -- | Get the 'User' representing the bot itself
  GetBotUser :: CacheEff m (Maybe User)
  -- | Set or Update a 'Guild' in the cache
  SetGuild :: Guild -> CacheEff m ()
  -- | Get a 'Guild' from the cache
  GetGuild :: Snowflake Guild -> CacheEff m (Maybe Guild)
  -- | Get a 'GuildChannel' from the cache
  GetGuildChannel :: Snowflake GuildChannel -> CacheEff m (Maybe GuildChannel)
  -- | Get all 'Guild's from the cache
  GetGuilds :: CacheEff m [Guild]
  -- | Delete a 'Guild' from the cache
  DelGuild :: Snowflake Guild -> CacheEff m ()
  -- | Set or Update a 'DMChannel' in the cache
  SetDM :: DMChannel -> CacheEff m ()
  -- | Get a 'DMChannel' from the cache
  GetDM :: Snowflake DMChannel -> CacheEff m (Maybe DMChannel)
  -- | Get all 'DMChannel's from the cache
  GetDMs :: CacheEff m [DMChannel]
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
  -- | Get all 'User's from the cache
  GetUsers :: CacheEff m [User]
  -- | Delete a 'User' from the cache
  DelUser :: Snowflake User -> CacheEff m ()
  -- | Flag a 'Guild' as unavailable
  SetUnavailableGuild :: Snowflake Guild -> CacheEff m ()
  -- | Test if a 'Guild' is flagged as unavailable
  IsUnavailableGuild :: Snowflake Guild -> CacheEff m Bool
  -- | Get all 'UnavailableGuild's from the cache
  GetUnavailableGuilds :: CacheEff m [Snowflake Guild]
  -- | Unflag a 'Guild' from being unavailable
  DelUnavailableGuild :: Snowflake Guild -> CacheEff m ()
  -- | Add or Update a 'Message' in the cache
  SetMessage :: Message -> CacheEff m ()
  -- | Get a 'Message' from the cache
  GetMessage :: Snowflake Message -> CacheEff m (Maybe Message)
  -- | Get all 'Message's from the cache
  GetMessages :: CacheEff m [Message]
  -- | Delete a 'Message' from the cache
  DelMessage :: Snowflake Message -> CacheEff m ()

makeSem ''CacheEff

updateBotUser :: (P.Member CacheEff r) => (User -> User) -> Sem r ()
updateBotUser f = getBotUser >>= flip whenJust (setBotUser . f)

updateGuild :: (P.Member CacheEff r) => Snowflake Guild -> (Guild -> Guild) -> Sem r ()
updateGuild id f = getGuild id >>= flip whenJust (setGuild . f)

updateDM :: (P.Member CacheEff r) => Snowflake DMChannel -> (DMChannel -> DMChannel) -> Sem r ()
updateDM id f = getDM id >>= flip whenJust (setDM . f)

updateUser :: (P.Member CacheEff r) => Snowflake User -> (User -> User) -> Sem r ()
updateUser id f = getUser id >>= flip whenJust (setUser . f)

updateMessage :: (P.Member CacheEff r) => Snowflake Message -> (Message -> Message) -> Sem r ()
updateMessage id f = getMessage id >>= flip whenJust (setMessage . f)
