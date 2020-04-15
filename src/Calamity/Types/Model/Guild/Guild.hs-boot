-- | Discord Guilds
module Calamity.Types.Model.Guild.Guild
    ( Guild
    --, Partial(Guild)
    , UpdatedGuild ) where

data Guild

instance Show Guild
instance Eq Guild

-- data instance Partial Guild = PartialGuild
--   { id   :: !(Snowflake Guild)
--   , name :: !ShortText
--   }
--   deriving ( Eq, Show, Generic )
--   deriving ( ToJSON, FromJSON ) via CalamityJSON (Partial Guild)
--   deriving ( HasID ) via HasIDFieldAlt (Partial Guild) Guild

data UpdatedGuild

instance Show UpdatedGuild
instance Eq UpdatedGuild
