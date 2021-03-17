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
, license = "Apache-2.0"
, packages = ./packages.dhall
, repository = "https://github.com/citizennet/purescript-fuzzy.git"
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
