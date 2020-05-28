{-# LANGUAGE NoDeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Discord gateway intents
module Calamity.Gateway.Intents
    ( Intents(..)
    , intentGuilds
    , intentGuildMembers
    , intentGuildBans
    , intentGuildEmojis
    , intentGuildIntegrations
    , intentGuildWebhooks
    , intentGuildInvites
    , intentGuildVoiceStates
    , intentGuildPresences
    , intentGuildMessages
    , intentGuildMessageReactions
    , intentGuildMessageTyping
    , intentDirectMessages
    , intentDirectMessageReactions
    , intentDirectMessageTyping ) where

import           Data.Aeson    ( ToJSON )
import           Data.Flags    ()
import           Data.Flags.TH
import           Data.Word

$(bitmaskWrapper "Intents" ''Word32 []
   [ ("intentGuilds", 0)
   , ("intentGuildMembers", 1)
   , ("intentGuildBans", 2)
   , ("intentGuildEmojis", 3)
   , ("intentGuildIntegrations", 4)
   , ("intentGuildWebhooks", 5)
   , ("intentGuildInvites", 6)
   , ("intentGuildVoiceStates", 7)
   , ("intentGuildPresences", 8)
   , ("intentGuildMessages", 9)
   , ("intentGuildMessageReactions", 10)
   , ("intentGuildMessageTyping", 11)
   , ("intentDirectMessages", 12)
   , ("intentDirectMessageReactions", 13)
   , ("intentDirectMessageTyping", 14)])

deriving via Word32 instance ToJSON Intents
