module Test.Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.AVar (AVAR)
import Control.Monad.Eff.Console (CONSOLE)
import Data.Either (Either(..))
import Data.Fuzzy (Fuzzy(..), FuzzyStr(..), Rank(..), match, matchStr)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..))
import Data.StrMap (StrMap, fromFoldable)
import Data.Tuple (Tuple(..))
import Test.Unit (suite, test)
import Test.Unit.Assert (equal)
import Test.Unit.Console (TESTOUTPUT)
import Test.Unit.Main (runTest)

newtype TestRecord = TR { name :: String, value :: String }
derive instance genericTestRecord :: Generic TestRecord _
instance eqTestRecord :: Eq TestRecord where eq = genericEq
instance showTestRecord :: Show TestRecord where show = genericShow

main ::
  ∀ e
  . Eff
    ( console :: CONSOLE
    , testOutput :: TESTOUTPUT
    , avar :: AVAR
    )
    Unit
main = runTest do
  suite "matchStr" do
    test "matches empty pattern" do
      let result = matchStr false "" "does not matter"
      let expected = Just $ FuzzyStr { result: [ Left "does not matter" ]
                                     , score: None
                                     }
      equal expected result

    test "pattern matches full string" do
      let result = matchStr false "foo bar" "foo bar"
      let expected = Just $ FuzzyStr { result: [ Right "foo bar" ]
                                     , score: Rank 0 0 0 0 0 0
                                     }
      equal expected result

    test "full pattern matches inside larger string" do
      let result = matchStr false "foo bar" "fiz baz foo bar buz"
      let expected = Just $ FuzzyStr { result: [ Left  "fiz baz "
                                               , Right "foo bar"
                                               , Left  " buz"
                                               ]
                                     , score: Rank 0 0 0 8 0 4
                                     }
      equal expected result

    test "full words in pattern match inside string" do
      let result = matchStr false "foo bar" "food barn"
      let expected = Just $ FuzzyStr { result: [ Right "foo"
                                               , Left  "d "
                                               , Right "bar"
                                               , Left  "n"
                                               ]
                                     , score: Rank 1 0 0 2 1 1
                                     }
      equal expected result

    test "all chars in pattern match inside string" do
      let result = matchStr false "foo bar" "a foul oat bear table"
      let expected = Just $ FuzzyStr { result: [ Left  "a "
                                               , Right "fo"
                                               , Left  "ul "
                                               , Right "o"
                                               , Left  "at "
                                               , Right "b"
                                               , Left  "e"
                                               , Right "ar"
                                               , Left  " table"
                                               ]
                                     , score: Rank 3 4 0 5 0 6
                                     }
      equal expected result

    test "last of two words and rest of chars in pattern match string" do
      let result = matchStr false "foo bar" "flavor of barnacle"
      let expected = Just $ FuzzyStr { result: [ Right "f"
                                               , Left  "lav"
                                               , Right "o"
                                               , Left  "r "
                                               , Right "o"
                                               , Left  "f "
                                               , Right "bar"
                                               , Left  "nacle"
                                               ]
                                     , score: Rank 2 5 0 2 5 5
                                     }
      equal expected result

    test "first of two words and rest of chars in pattern match string" do
      let result = matchStr false "foo bar" "food abean wars"
      let expected = Just $ FuzzyStr { result: [ Right "foo"
                                               , Left  "d a"
                                               , Right "b"
                                               , Left  "e"
                                               , Right "a"
                                               , Left  "n wa"
                                               , Right "r"
                                               , Left  "s"
                                               ]
                                     , score: Rank 2 5 1 3 1 1
                                     }
      equal expected result

    test "only first word in pattern matches" do
      let result = matchStr false "foo bar" "bar afood"
      let expected = Just $ FuzzyStr { result: [ Left  "bar a"
                                               , Right "foo"
                                               , Left  "d"
                                               ]
                                     , score: Rank 5 0 1 5 1 1
                                     }
      equal expected result

    test "only some chars in pattern match" do
      let result = matchStr false "foo bar" "good burn"
      let expected = Just $ FuzzyStr { result: [ Left  "g"
                                               , Right "oo"
                                               , Left  "d "
                                               , Right "b"
                                               , Left  "u"
                                               , Right "r"
                                               , Left  "n"
                                               ]
                                     , score: Rank 5 1 1 3 1 1
                                     }
      equal expected result

    test "only one char in pattern matches" do
      let result = matchStr false "foo bar" "standing"
      let expected = Just $ FuzzyStr { result: [ Left  "st"
                                               , Right "a"
                                               , Left  "nding"
                                               ]
                                     , score: Rank 8 0 2 2 5 5
                                     }
      equal expected result

    test "none of pattern matches" do
      let result = matchStr false "foo bar" "FOO BAR"
      equal Nothing result

    test "full pattern matches, ignoring case" do
      let result = matchStr true "foo bar" "FOO BAR"
      let expected = Just $ FuzzyStr { result: [ Right "FOO BAR" ]
                                     , score: Rank 0 0 0 0 0 0
                                     }
      equal expected result

    test "some chars in pattern match, ignoring case" do
      let result = matchStr true "foo bar" "GoOd Barn"
      let expected = Just $ FuzzyStr { result: [ Left  "G"
                                               , Right "oO"
                                               , Left  "d "
                                               , Right "Bar"
                                               , Left  "n"
                                               ]
                                     , score: Rank 3 0 1 3 1 1
                                     }
      equal expected result

  suite "match" do
    test "match pattern in record keys" do
      let original = TR { name: "Foo Bar", value: "foobar" }
      let result = match true toMapStr "foo bar" original
      let expected = Just $ Fuzzy { original
                                  , result: fromFoldable
                                    [ Tuple "name" (Just [ Right "Foo Bar" ])
                                    , Tuple "value" (Just [ Right "foobar" ])
                                    ]
                                  , score: Rank 0 0 0 0 0 0
                                  }
      equal expected result

    test "partially match pattern in one of record keys" do
      let original = TR { name: "Flood Gates", value: "123456788" }
      let result = match true toMapStr "foo bar" original
      let expected = Just $ Fuzzy { original
                                  , result: fromFoldable
                                    [ Tuple "name" (Just [ Right "F"
                                                         , Left  "l"
                                                         , Right "oo"
                                                         , Left  "d G"
                                                         , Right "a"
                                                         , Left  "tes"
                                                         ]
                                                    )
                                    , Tuple "value" Nothing
                                    ]
                                  , score: Rank 5 1 1 3 3 3
                                  }
      equal expected result

    test "no pattern match in any keys" do
      let original = TR { name: "FOO BAR", value: "FOOBAR" }
      let result = match false toMapStr "foo bar" original
      equal Nothing result
  where
    toMapStr :: TestRecord -> StrMap String
    toMapStr (TR { name, value }) = fromFoldable [ Tuple "name" name, Tuple "value" value ]
