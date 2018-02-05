module Test.Main where

import Prelude

import Data.Either (Either(..))
import Data.Fuzzy (FuzzyStr(..), Rank(..), Pos(..), match')
import Data.Maybe (Maybe(..))
import Test.Unit (suite, test)
import Test.Unit.Assert (assert, equal)
import Test.Unit.Main (runTest)

main = runTest do
  suite "match'" do
    test "matches empty pattern" do
      let result = match' false "" "does not matter"
      let expected = Just $ FuzzyStr { result: [ Left "does not matter" ]
                                     , score: None
                                     }
      equal result expected

    test "matches full string" do
      let result = match' false "foobar" "foobar"
      let expected = Just $ FuzzyStr { result: [ Right "foobar" ]
                                     , score: Rank 0 0 0 0
                                     }
      equal result expected

    test "matches broken string" do
      let result = match' false "foobar" "foo bar"
      let expected = Just $ FuzzyStr { result: [ Right "foo", Left " ", Right "bar" ]
                                     , score: Rank 1 0 0 0
                                     }
      equal result expected

    test "matches broken string with trailing words" do
      let result = match' false "foobar" "foo bar foo bar"
      let expected = Just $ FuzzyStr { result: [ Right "foo"
                                               , Left " "
                                               , Right "bar"
                                               , Left " foo bar"
                                               ]
                                     , score: Rank 1 0 0 8
                                     }
      equal result expected

    test "matches broken string with preceding text and trailing words" do
      let result = match' false "foobar" "bar foo bar foo bar"
      let expected = Just $ FuzzyStr { result: [ Left "bar "
                                               , Right "foo"
                                               , Left " "
                                               , Right "bar"
                                               , Left " foo bar"
                                               ]
                                     , score: Rank 1 4 0 8
                                     }
      equal result expected

    test "matches broken, partial string with preceding text and trailing words" do
      let result = match' false "foobar" "barts foo barts foo barts"
      let expected = Just $ FuzzyStr { result: [ Left "barts "
                                               , Right "foo"
                                               , Left " "
                                               , Right "bar"
                                               , Left "ts foo barts"
                                               ]
                                     , score: Rank 1 6 2 12
                                     }
      equal result expected

    test "matches spread out, partial string with preceding text and trailing words" do
      let result = match' false "foo" "one of two and four"
      let expected = Just $ FuzzyStr { result: [ Left "one o"
                                               , Right "f"
                                               , Left " tw"
                                               , Right "o"
                                               , Left " and f"
                                               , Right "o"
                                               , Left "ur"
                                               ]
                                     , score: Rank 9 5 2 2
                                     }
      equal result expected

    test ( "does not match spread out, partial string with preceding text and trailing words, "
        <> "respecting case"
        )  do
      let result = match' false "Foo" "one of TWO and FOUR"
      equal result Nothing

    test ( "matches spread out, partial string with preceding text and trailing words, "
        <> "ignoring case"
        )  do
      let result = match' true "Foo" "one of TWO and FOUR"
      let expected = Just $ FuzzyStr { result: [ Left "one o"
                                               , Right "f"
                                               , Left " TW"
                                               , Right "O"
                                               , Left " and F"
                                               , Right "O"
                                               , Left "UR"
                                               ]
                                     , score: Rank 9 5 2 2
                                     }
      equal result expected

    test "does not match string" do
      let result = match' false "foo" "one of two and three"
      equal result Nothing
