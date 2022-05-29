# Changelog for Calamity Commands

## 0.3.0.0

+ DSL commands no longer use a concrete effect list prefix.

## 0.2.0.0

+ Remove all usages of lazy Text (except from typeclass instances)

## 0.1.3.0

+ Fixed some parameter parser instances causing type inference to fail
  ([#48](https://github.com/simmsb/calamity/pull/48)).

## 0.1.2.0

+ Changed the command parameter machinery to hold more info about parameters.
+ Added a 'type cheatsheet' thing to command help in the default help command.
+ The `render` parameter of `helpCommand` and `helpCommand'` is now passed the
  context.
+ `ParameterParser` now has an extra type variable `c` to parameterise the
  context type.

## 0.1.1.0

+ The minimum version of `base` has been upped to `4.13` as the library fails to
  build on ghc-8.6
+ The minimum version of `polysemy` has been upped to 1.5
+ The upper bound of `lens` has been bumped to 6

## 0.1.0.0

Initial release of the library

## Unreleased changes
