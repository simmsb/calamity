{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoDeriveAnyClass #-}

-- | Discord gateway intents
module Calamity.Gateway.Intents (
    Intents (..),
    defaultIntents,
    intentGuilds,
    intentGuildMembers,
    intentGuildBans,
    intentGuildEmojis,
    intentGuildIntegrations,
    intentGuildWebhooks,
    intentGuildInvites,
    intentGuildVoiceStates,
    intentGuildPresences,
    intentGuildMessages,
    intentGuildMessageReactions,
    intentGuildMessageTyping,
    intentDirectMessages,
    intentDirectMessageReactions,
    intentDirectMessageTyping,
) where

import Data.Aeson (ToJSON)
import Data.Bits (Bits (shiftL))
import Data.Default.Class (Default (..))
import Data.Flags (allBut, (.+.))
import Data.Flags.TH (bitmaskWrapper)
import Data.Word (Word32)

$( bitmaskWrapper
    "Intents"
    ''Word32
    []
    [ ("intentGuilds", 1 `shiftL` 0)
    , ("intentGuildMembers", 1 `shiftL` 1)
    , ("intentGuildBans", 1 `shiftL` 2)
    , ("intentGuildEmojis", 1 `shiftL` 3)
    , ("intentGuildIntegrations", 1 `shiftL` 4)
    , ("intentGuildWebhooks", 1 `shiftL` 5)
    , ("intentGuildInvites", 1 `shiftL` 6)
    , ("intentGuildVoiceStates", 1 `shiftL` 7)
    , ("intentGuildPresences", 1 `shiftL` 8)
    , ("intentGuildMessages", 1 `shiftL` 9)
    , ("intentGuildMessageReactions", 1 `shiftL` 10)
    , ("intentGuildMessageTyping", 1 `shiftL` 11)
    , ("intentDirectMessages", 1 `shiftL` 12)
    , ("intentDirectMessageReactions", 1 `shiftL` 13)
    , ("intentDirectMessageTyping", 1 `shiftL` 14)
    , ("intentGuildScheduledEvents", 1 `shiftL` 16)
    ]
 )

deriving via Word32 instance ToJSON Intents

-- | Default intents are all but the privileged intents: members and intents
defaultIntents :: Intents
defaultIntents = allBut (intentGuildMembers .+. intentGuildPresences)

-- | Default intents are all but the privileged intents: members and intents
instance Default Intents where
    def = defaultIntents
