# Changelog for Calamity

## 0.1.26.1

+ Quick fix of GetChannelMessages

## 0.1.26.0

+ `GetChannelMessages` now has an extra parameter to allow the `limit` option to
  be applied at the same time as the other filters.
+ `ChannelMessagesQuery` has been renamed to `ChannelMessagesFilter`.
+ The `ChannelMessagesLimit` type has been introduced.

## 0.1.25.1

+ Add `Upgradeable` instances for `VoiceChannnel`, `DMChannel`_, `GroupChannel`, 
  `TextChannel`, and `Category`.

## 0.1.25.0

+ Changed how `ModifyGuildMemberData`, and `ModifyGuildRoleData` are implemented
  to allow for the parameters to be optional and nullable.
+ Changed `EditMessage` to use `EditMessageData` instead of `Maybe Text -> Maybe
  Embed`, allowing for the parameters to be optional and nullable.

## 0.1.24.2

+ Add event handlers for voice state update events: `'VoiceStateUpdateEvt`
+ Apply some fixes for a few memory leaks

## 0.1.24.1

+ Fix some memory leaks

## 0.1.24.0

+ Switch from using Wreq to Req
+ The `session` parameter has been removed from `runBotIO'`
+ Add an `Upgradeable` instance for `Role`s
+ Add a command `Parser` instance for `Role`s

## 0.1.23.1

+ Fix some more json parsing issues

## 0.1.23.0

+ The `roles` field was incorrectly present on the `PresenceUpdate` type, that
  field has been removed.
+ The `game` field on `Presence` was changed to `activities :: [Activity]`

## 0.1.22.1

+ Bump some upper bounds
+ Fix parsing of members from GetGuildMember
+ Make game field in Presence correctly optional
+ Make roles field in Presence updates correctly optional (internal)

## 0.1.22.0

* Update to gateway/http endpoint v8.
* Updated the message types.
* Added `defaultIntents`, which is all but the privileged intents, also a
  Data.Default instance.
* `runBotIO` and `runBotIO'` now always take an `Intents` parameter.
* Users are cached from messages, as well as member create events.
* Fix `Overwrite`s having an incorrect `type_` field.

## 0.1.21.0

* Fix ToJSON instance for `RawEmoji`

## 0.1.20.1

* Documentation improvements.

## 0.1.20.0

* Migrate do di-polysemy 0.2, runBotIO no longer handles the Log effect.

## 0.1.19.2

* Fix broken `Float` parsing.

## 0.1.19.1

* Fixed another missing activity type

## 0.1.19.0

* Support hidden commands.

## 0.1.18.1

* Fix custom presences failing to parse.

## 0.1.18.0

* Add raw message events: `RawMessageUpdateEvt`, `RawMessageDeleteEvt`,
  `RawMessageDeleteBulkEvt`, `RawMessageReactionAddEvt`,
  `RawMessageReactionRemoveEvt`, `RawMessageReactionRemoveAllEvt`.

* Fixed bulk message deletes firing a message delete per deleted message,
  instead of a bulk message delete event (I'm not sure how I did that).

* Add `animated` field to `Partial Emoji`s.

* Make show instances for `Partial Emoji` and `RawEmoji` show to their discord
  representation.

## 0.1.17.2

*2020-07-04*

* Drop GHC-8.6.5, it doesn't like strictdata

* Use unboxing-vector instead of doing stuff ourselves

## 0.1.17.1

*2020-06-29*

* `waitUntil` and `waitUntilM` now correctly remove the temorary event handler
  they create if an exception is raised.

## 0.1.17.0

*2020-06-28*

* Allow the session used for http requests to be specified to the client.

* Drop from using a Wreq fork to vanilla Wreq.

* `TFile` now requires a filename parameter.

## 0.1.16.0

* Change how commands should be manually invoked from code, instead of firing a
  `"invoke-command"` custom event, now the `handleCommands` function should be
  used, which returns information about if the command succeeded.

* Added `fetchHandler` for retrieving the command handler inside a command DSL.

## 0.1.15.0

* General cleanup of codebase

* Enable StrictData by default

## 0.1.14.9

*2020-06-22*

* Support manually invoking commands.

## 0.1.14.8

*2020-06-21*

* Replace uses of withLowerToIO with interpretFinal (should be more performant)

## 0.1.14.7

*2020-06-21*

* Fix missing usage of GetAuditLogOptions in GetAuditLog

## 0.1.14.6

*2020-06-18*

* Add command parameter `Parser`s for `Int`, `Integer`, `Word`, `Natural`, and
  `Float`.

## 0.1.14.5

*2020-06-18*

* The `DecodeError` variant of the `RestError` type has been renamed to
  `InternalClientError` as all issues in the rest client now end up here.

* We're now using `discord.com` instead of `discordapp.com`

## 0.1.14.4

*2020-06-11*

* Added `activity` to construct Activities

* Added aliases for commands and groups, with new functions to create them
  (`commandA`, `groupA`, ...).

* The built in help command now shows aliases and checks.

## 0.1.14.3

*2020-06-10*

* Fix some bugs in the gateway

## 0.1.14.2

*2020-06-09*

* Fix broken json decoding for DMs

* Add `mentionChannels` to `Message`

## 0.1.14.1

*2020-06-08*

* Fix broken json decoding for member's

## 0.1.14.0

*2020-06-08*

* Unpacked the `user` field of `Member` into itself.

* Add message formatting utilities (`Calamity.Utils.Message`).

* Add support for allowed mentions in `Tellable`.

* Change Snowflake's show instance to just show the numberic id.

* Added parsers for RawEmoji and Either.

## 0.1.13.0

*2020-06-06*

* Changed event handlers to take tuples instead of being higher arity when there
  is more than one parameter to the callback.

## 0.1.12.0

*2020-06-06*

* Changed some events to take enums instead of booleans: `GuildCreateEvt` and
  `GuildDeleteEvt`.

## 0.1.11.2

*2020-06-03*

* Moved the internal `UpdatedMessage` from
  `Calamity.Types.Model.Channel.Message` into
  `Calamity.Types.Model.Channel.UpdatedMessage`.

## 0.1.11.0

*2020-05-31*

* Add command parameter parsers for channel/guild/emoji
* Support allowed mentions
* Support invite events
* Support setting gateway intents
* Add `Calamity.Types.Model.Guild.Permission` and `Calamity.Utils.Permissions`
  and change permissions fields from `Word64` to `Permissions`
* Add `Calamity.Utils.Colour` and change color fields from `Word64` to
  `Data.Color.Color Double`

## 0.1.10.0

*2020-05-27*

* Renamed `Calamity.Commands.Parser.KleeneConcat` to
  `Calamity.Commands.Parser.KleeneStarConcat` and added
  `Calamity.Commands.Parser.KleenePlusConcat`

* Added `Calamity.Types.Upgradeable`

## 0.1.9.2

*2020-05-23*

* Added a default help command, located in `Calamity.Commands.Help`.

* Commands now have the list of parameters they take

## 0.1.9.1

*2020-05-23*

* Added `Calamity.Commands.Parser.Named` for parameters that have a name.

* General improvements to parser errors

## 0.1.9.0

*2020-05-22*

* Added commands, located in `Calamity.Commands`, along with a DSL for declaring
  commands nicely.

* Renamed `waitUntil` to `waitUntilM`, and introduced a variant with a pure
  check function that takes the original name of `waitUntil`.

## 0.1.8.0

*2020-05-15*

* Did a large rework of how event handlers are stored internally.
* introduced `waitUntil`

## 0.1.4.0

* Added back extra exports of `Calamity.Types.Partial` from
  `Calamity.Types.Model.Guild.Guild`, `Calamity.Type.Model.Guild.Emoji`, and
  `Calamity.Types.Model.Channel`. There is now way to export the constructor
  without also exporting Partial apparently?


## 0.1.3.0

*2020-04-27*

* Removed extra exports of `Calamity.Types.Partial` from
  `Calamity.Types.Model.Guild.Guild`, `Calamity.Type.Model.Guild.Emoji`, and
  `Calamity.Types.Model.Channel`

* Added missing exports of `CreateGuildEmojiOptions` and
  `ModifyGuildEmojiOptions` from `Calamity.HTTP.Emoji`

* Added missing exports of `CreateGuildData` and `ModifyGuildData` from
  `Calamity.HTTP.Guild`

## 0.1.2.0

*2020-04-27*

* Calamity.Client: runBotIO now has a `Polysemy.Fail` effect

## Unreleased changes
