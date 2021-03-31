{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoDeriveAnyClass #-}

-- | Guild permissions
module Calamity.Types.Model.Guild.Permissions
  ( Permissions (..),
    createInstantInvite,
    kickMembers,
    banMembers,
    administrator,
    manageChannels,
    manageGuild,
    addReactions,
    viewAuditLog,
    prioritySpeaker,
    stream,
    viewChannel,
    sendMessages,
    sendTtsMessages,
    manageMessages,
    embedLinks,
    attachFiles,
    readMessageHistory,
    mentionEveryone,
    useExternalEmojis,
    viewGuildInsights,
    connect,
    speak,
    muteMembers,
    deafenMembers,
    moveMembers,
    useVad,
    changeNickname,
    manageNicknames,
    manageRoles,
    manageWebhooks,
    manageEmojis,
  )
where

import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Aeson.Types (parseFail)
import Data.Flags ()
import Data.Flags.TH
import Data.Text.Read (decimal)
import Data.Word
import TextShow
import Control.DeepSeq (NFData)

$( bitmaskWrapper
     "Permissions"
     ''Word64
     []
     [ ("createInstantInvite", 0x00000001),
       ("kickMembers", 0x00000002),
       ("banMembers", 0x00000004),
       ("administrator", 0x00000008),
       ("manageChannels", 0x00000010),
       ("manageGuild", 0x00000020),
       ("addReactions", 0x00000040),
       ("viewAuditLog", 0x00000080),
       ("prioritySpeaker", 0x00000100),
       ("stream", 0x00000200),
       ("viewChannel", 0x00000400),
       ("sendMessages", 0x00000800),
       ("sendTtsMessages", 0x00001000),
       ("manageMessages", 0x00002000),
       ("embedLinks", 0x00004000),
       ("attachFiles", 0x00008000),
       ("readMessageHistory", 0x00010000),
       ("mentionEveryone", 0x00020000),
       ("useExternalEmojis", 0x00040000),
       ("viewGuildInsights", 0x00080000),
       ("connect", 0x00100000),
       ("speak", 0x00200000),
       ("muteMembers", 0x00400000),
       ("deafenMembers", 0x00800000),
       ("moveMembers", 0x01000000),
       ("useVad", 0x02000000),
       ("changeNickname", 0x04000000),
       ("manageNicknames", 0x08000000),
       ("manageRoles", 0x10000000),
       ("manageWebhooks", 0x20000000),
       ("manageEmojis", 0x4000000)
     ]
 )

instance ToJSON Permissions where
  toJSON = toJSON . showt
  toEncoding = toEncoding . showt

instance FromJSON Permissions where
  parseJSON a = do
    asText <- parseJSON a
    case decimal asText of
      Right (n, _) -> pure $ Permissions n
      Left e -> parseFail e

deriving via FromStringShow Permissions instance TextShow Permissions
deriving via Word64 instance NFData Permissions
