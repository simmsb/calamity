# Changelog for Calamity

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
