module Test.Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.AVar (AVAR)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Random (RANDOM)
import Data.Array (sort)
import Data.Either (Either(..))
import Data.Fuzzy (Fuzzy(..), FuzzyStr(..), Distance(..), match, matchStr)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Show (genericShow)
import Data.Rational ((%))
import Data.StrMap (StrMap, fromFoldable)
import Data.Tuple (Tuple(..))
import Test.QuickCheck.Laws (checkLaws)
import Test.QuickCheck.Laws.Data (checkMonoid, checkSemigroup)
import Test.Unit (suite, test)
import Test.Unit.Assert (equal)
import Test.Unit.Console (TESTOUTPUT)
import Test.Unit.Main (runTest)
import Type.Proxy (Proxy(..))

newtype TestRecord = TR { name :: String, value :: String }
derive instance genericTestRecord :: Generic TestRecord _
instance eqTestRecord :: Eq TestRecord where eq = genericEq
instance showTestRecord :: Show TestRecord where show = genericShow

checkDistance ::
  ∀ e
  . Eff
    ( console :: CONSOLE
    , random :: RANDOM
    , exception :: EXCEPTION
    | e
    )
    Unit
checkDistance = checkLaws "Distance" do
  checkSemigroup prxDistance
  checkMonoid prxDistance
  where
    prxDistance = Proxy :: Proxy Distance

main ::
  ∀ e
  . Eff
    ( console :: CONSOLE
    , testOutput :: TESTOUTPUT
    , avar :: AVAR
    , exception :: EXCEPTION
    , random :: RANDOM
    | e
    )
    Unit
main = runTest do
  suite "laws" do
    test "Distance abides typeclass laws" $ liftEff checkDistance

  suite "matchStr" do
    test "matches empty pattern" do
      let result = matchStr false "" "does not matter"
      let expected = FuzzyStr { original: "does not matter"
                              , result: [ Left "does not matter" ]
                              , distance: None
                              , ratio: 1 % 1
                              }
      equal expected result

    test "pattern matches full string" do
      let result = matchStr false "foo bar" "foo bar"
      let expected = FuzzyStr { original: "foo bar"
                              , result: [ Right "foo bar" ]
                              , distance: Distance 0 0 0 0 0 0
                              , ratio: 1 % 1
                              }
      equal expected result

    test "full pattern matches inside larger string" do
      let result = matchStr false "foo bar" "fiz baz foo bar buz"
      let expected = FuzzyStr { original: "fiz baz foo bar buz"
                              , result: [ Left  "fiz baz "
                                        , Right "foo bar"
                                        , Left  " buz"
                                        ]
                              , distance: Distance 0 0 0 8 0 4
                              , ratio: 1 % 1
                              }
      equal expected result

    test "full words in pattern match inside string" do
      let result = matchStr false "foo bar" "food barn"
      let expected = FuzzyStr { original: "food barn"
                              , result: [ Right "foo"
                                        , Left  "d "
                                        , Right "bar"
                                        , Left  "n"
                                        ]
                              , distance: Distance 1 0 0 2 1 1
                              , ratio: 1 % 1
                              }
      equal expected result

    test "all chars in pattern match inside string" do
      let result = matchStr false "foo bar" "a foul oat bear table"
      let expected = FuzzyStr { original: "a foul oat bear table"
                              , result: [ Left  "a "
                                        , Right "fo"
                                        , Left  "ul "
                                        , Right "o"
                                        , Left  "at "
                                        , Right "b"
                                        , Left  "e"
                                        , Right "ar"
                                        , Left  " table"
                                        ]
                              , distance: Distance 3 4 0 5 0 6
                              , ratio: 1 % 1
                              }
      equal expected result

    test "last of two words and rest of chars in pattern match string" do
      let result = matchStr false "foo bar" "flavor of barnacle"
      let expected = FuzzyStr { original: "flavor of barnacle"
                              , result: [ Right "f"
                                        , Left  "lav"
                                        , Right "o"
                                        , Left  "r "
                                        , Right "o"
                                        , Left  "f "
                                        , Right "bar"
                                        , Left  "nacle"
                                        ]
                              , distance: Distance 2 5 0 2 5 5
                              , ratio: 1 % 1
                              }
      equal expected result

    test "first of two words and rest of chars in pattern match string" do
      let result = matchStr false "foo bar" "food abean wars"
      let expected = FuzzyStr { original: "food abean wars"
                              , result: [ Right "foo"
                                        , Left  "d a"
                                        , Right "b"
                                        , Left  "e"
                                        , Right "a"
                                        , Left  "n wa"
                                        , Right "r"
                                        , Left  "s"
                                        ]
                              , distance: Distance 2 5 1 3 1 1
                              , ratio: 1 % 1
                              }
      equal expected result

    test "only first word in pattern matches" do
      let result = matchStr false "foo bar" "bar afood"
      let expected = FuzzyStr { original: "bar afood"
                              , result: [ Left  "bar a"
                                        , Right "foo"
                                        , Left  "d"
                                        ]
                              , distance: Distance 5 0 1 5 1 1
                              , ratio: 1 % 2
                              }
      equal expected result

    test "only some chars in pattern match" do
      let result = matchStr false "foo bar" "good burn"
      let expected = FuzzyStr { original: "good burn"
                              , result: [ Left  "g"
                                        , Right "oo"
                                        , Left  "d "
                                        , Right "b"
                                        , Left  "u"
                                        , Right "r"
                                        , Left  "n"
                                        ]
                              , distance: Distance 5 1 1 3 1 1
                              , ratio: 2 % 3
                              }
      equal expected result

    test "only one char in pattern matches" do
      let result = matchStr false "foo bar" "standing"
      let expected = FuzzyStr { original: "standing"
                              , result: [ Left  "st"
                                        , Right "a"
                                        , Left  "nding"
                                        ]
                              , distance: Distance 8 0 2 2 5 5
                              , ratio: 1 % 6
                              }
      equal expected result

    test "none of pattern matches" do
      let result = matchStr false "foo bar" "FOO BAR"
      let expected = FuzzyStr { original: "FOO BAR"
                              , result: [ Left "FOO BAR" ]
                              , distance: Distance 9 0 0 0 0 0
                              , ratio: 0 % 1
                              }
      equal expected result

    test "full pattern matches, ignoring case" do
      let result = matchStr true "foo bar" "FOO BAR"
      let expected = FuzzyStr { original: "FOO BAR"
                              , result: [ Right "FOO BAR" ]
                              , distance: Distance 0 0 0 0 0 0
                              , ratio: 1 % 1
                              }
      equal expected result

    test "some chars in pattern match, ignoring case" do
      let result = matchStr true "foo bar" "GoOd Barn"
      let expected = FuzzyStr { original: "GoOd Barn"
                              , result: [ Left  "G"
                                        , Right "oO"
                                        , Left  "d "
                                        , Right "Bar"
                                        , Left  "n"
                                        ]
                              , distance: Distance 3 0 1 3 1 1
                              , ratio: 5 % 6
                              }
      equal expected result

  suite "match" do
    test "match pattern in record keys" do
      let original = TR { name: "Foo Bar", value: "foobar" }
      let result = match true toMapStr "foo bar" original
      let expected = Fuzzy { original
                           , result: fromFoldable
                             [ Tuple "name" [ Right "Foo Bar" ]
                             , Tuple "value" [ Right "foobar" ]
                             ]
                           , distance: Distance 0 0 0 0 0 0
                           , ratio: 1 % 1
                           }
      equal expected result

    test "partially match pattern in one of record keys" do
      let original = TR { name: "Flood Gates", value: "123456788" }
      let result = match true toMapStr "foo bar" original
      let expected = Fuzzy { original
                           , result: fromFoldable
                             [ Tuple "name" [ Right "F"
                                            , Left  "l"
                                            , Right "oo"
                                            , Left  "d G"
                                            , Right "a"
                                            , Left  "tes"
                                            ]
                             , Tuple "value" [ Left "123456788" ]
                             ]
                           , distance: Distance 5 1 1 3 3 3
                           , ratio: 2 % 3
                           }
      equal expected result

    test "no pattern match in any keys" do
      let original = TR { name: "FOO BAR", value: "FOOBAR" }
      let result = match false toMapStr "foo bar" original
      let expected = Fuzzy { original
                           , result: fromFoldable
                             [ Tuple "name" [ Left "FOO BAR" ]
                             , Tuple "value" [ Left "FOOBAR" ]
                             ]
                           , distance: Distance 9 0 0 0 0 0
                           , ratio: 0 % 1
                           }
      equal expected result

  suite "compare" do
    test "compare works as expected on FuzzyStr" do
      let matchFooBar = matchStr true "foo bar"
      let a = matchFooBar "f"
      let b = matchFooBar "Foo bar"
      let c = matchFooBar "Flood"
      let result = sort [a, b, c]
      let expected = [b, c, a]
      equal expected result

    test "compare works as expected on Fuzzy" do
      let matchFooBar = match true toMapStr "foo bar"
      let a = matchFooBar $ TR { name: "FAN", value: "fan" }
      let b = matchFooBar $ TR { name: "FAM", value: "fam" }
      let c = matchFooBar $ TR { name: "Foobar", value: "foo" }
      let result = sort [a, b, c]
      let expected = [c, a, b]
      equal expected result

  where
    toMapStr :: TestRecord -> StrMap String
    toMapStr (TR { name, value }) = fromFoldable [ Tuple "name" name, Tuple "value" value ]
