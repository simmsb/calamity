{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoDeriveAnyClass #-}

-- | Guild permissions
module Calamity.Types.Model.Guild.Permissions (
  Permissions (..),
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
) where

import Control.DeepSeq (NFData)
import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Aeson.Types (parseFail)
import Data.Bits (Bits (shiftL))
import Data.Flags ()
import Data.Flags.TH
import Data.Text.Read (decimal)
import Data.Word
import TextShow

$( bitmaskWrapper
    "Permissions"
    ''Word64
    []
    [ ("createInstantInvite", 1 `shiftL` 0)
    , ("kickMembers", 1 `shiftL` 1)
    , ("banMembers", 1 `shiftL` 2)
    , ("administrator", 1 `shiftL` 3)
    , ("manageChannels", 1 `shiftL` 4)
    , ("manageGuild", 1 `shiftL` 5)
    , ("addReactions", 1 `shiftL` 6)
    , ("viewAuditLog", 1 `shiftL` 7)
    , ("prioritySpeaker", 1 `shiftL` 8)
    , ("stream", 1 `shiftL` 9)
    , ("viewChannel", 1 `shiftL` 10)
    , ("sendMessages", 1 `shiftL` 11)
    , ("sendTtsMessages", 1 `shiftL` 12)
    , ("manageMessages", 1 `shiftL` 13)
    , ("embedLinks", 1 `shiftL` 14)
    , ("attachFiles", 1 `shiftL` 15)
    , ("readMessageHistory", 1 `shiftL` 16)
    , ("mentionEveryone", 1 `shiftL` 17)
    , ("useExternalEmojis", 1 `shiftL` 18)
    , ("viewGuildInsights", 1 `shiftL` 19)
    , ("connect", 1 `shiftL` 20)
    , ("speak", 1 `shiftL` 21)
    , ("muteMembers", 1 `shiftL` 22)
    , ("deafenMembers", 1 `shiftL` 23)
    , ("moveMembers", 1 `shiftL` 24)
    , ("useVad", 1 `shiftL` 25)
    , ("changeNickname", 1 `shiftL` 26)
    , ("manageNicknames", 1 `shiftL` 27)
    , ("manageRoles", 1 `shiftL` 28)
    , ("manageWebhooks", 1 `shiftL` 29)
    , ("manageEmojis", 1 `shiftL` 30)
    , ("useApplicationCommands", 1 `shiftL` 31)
    , ("requestToSPeak", 1 `shiftL` 32)
    , ("manageEvents", 1 `shiftL` 33)
    , ("manageThreads", 1 `shiftL` 34)
    , ("createPublicThreads", 1 `shiftL` 35)
    , ("createPrivateThreads", 1 `shiftL` 36)
    , ("useExternalStickers", 1 `shiftL` 37)
    , ("sendMessagesInThreads", 1 `shiftL` 38)
    , ("useEmbeddedActivities", 1 `shiftL` 39)
    , ("moderateMembers", 1 `shiftL` 40)
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
