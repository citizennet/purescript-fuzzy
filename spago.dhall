{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "fuzzy"
, dependencies =
  [ "console"
  , "datetime"
  , "effect"
  , "foldable-traversable"
  , "foreign-object"
  , "newtype"
  , "ordered-collections"
  , "prelude"
  , "psci-support"
  , "rationals"
  , "strings"
  , "test-unit"
  , "tuples"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
