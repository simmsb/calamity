# Changelog for Calamity

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
