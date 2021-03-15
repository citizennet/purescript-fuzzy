module Test.Main where

import Prelude

import Effect (Effect)
import Data.Array (find, reverse, sort)
import Data.Date (Date, Day, Month(..), Year, canonicalDate, day, month, weekday, year)
import Data.DateTime (adjust, date)
import Data.DateTime.Instant (fromDate, toDateTime)
import Data.Either (Either(..))
import Data.Enum (class Enum, fromEnum, pred, succ, toEnum)
import Data.Fuzzy (Fuzzy(..), FuzzyStr(..), Distance(..), match, matchStr)
import Data.Generic.Rep (class Generic)
import Data.Eq.Generic (genericEq)
import Data.Show.Generic (genericShow)
import Data.Maybe (Maybe(..), fromJust)
import Data.Newtype (class Newtype)
import Data.Rational ((%))
import Foreign.Object (Object, fromFoldable)
import Data.Time.Duration (Days(..))
import Data.Tuple (Tuple(..))
import Partial.Unsafe (unsafePartial)
import Test.QuickCheck.Arbitrary (arbitrary, class Arbitrary)
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert (equal)
import Test.Unit.QuickCheck (quickCheck')
import Test.Unit.Main (runTest)

newtype TestRecord = TR { name :: String, value :: String }
derive instance genericTestRecord :: Generic TestRecord _
instance eqTestRecord :: Eq TestRecord where eq = genericEq
instance showTestRecord :: Show TestRecord where show = genericShow

main :: Effect Unit
main = runTest do
  suite "laws" do
    checkSemigroup
    checkMonoid

  suite "matchStr" do
    test "matches empty pattern" do
      let result = matchStr false "" "does not matter"
      let expected = FuzzyStr { original: "does not matter"
                              , segments: [ Left "does not matter" ]
                              , distance: None
                              , ratio: 1 % 1
                              }
      equal expected result

    test "pattern matches full string" do
      let result = matchStr false "foo bar" "foo bar"
      let expected = FuzzyStr { original: "foo bar"
                              , segments: [ Right "foo bar" ]
                              , distance: Distance 0 0 0 0 0 0
                              , ratio: 1 % 1
                              }
      equal expected result

    test "full pattern matches inside larger string" do
      let result = matchStr false "foo bar" "fiz baz foo bar buz"
      let expected = FuzzyStr { original: "fiz baz foo bar buz"
                              , segments: [ Left  "fiz baz "
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
                              , segments: [ Right "foo"
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
                              , segments: [ Left  "a "
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
                              , segments: [ Right "f"
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
                              , segments: [ Right "foo"
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
                              , segments: [ Left  "bar a"
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
                              , segments: [ Left  "g"
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
                              , segments: [ Left  "st"
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
                              , segments: [ Left "FOO BAR" ]
                              , distance: Distance 9 0 0 0 0 0
                              , ratio: 0 % 1
                              }
      equal expected result

    test "full pattern matches, ignoring case" do
      let result = matchStr true "foo bar" "FOO BAR"
      let expected = FuzzyStr { original: "FOO BAR"
                              , segments: [ Right "FOO BAR" ]
                              , distance: Distance 0 0 0 0 0 0
                              , ratio: 1 % 1
                              }
      equal expected result

    test "some chars in pattern match, ignoring case" do
      let result = matchStr true "foo bar" "GoOd Barn"
      let expected = FuzzyStr { original: "GoOd Barn"
                              , segments: [ Left  "G"
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
                           , segments: fromFoldable
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
                           , segments: fromFoldable
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
                           , segments: fromFoldable
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

  suite "date" do
    test "match nearest wednesday" do
      let matchWed = match true dateToMapStr "wed"
      let result = firstMatchingDate $ matchWed <$> dates
      let expected = Just $ matchWed $ unsafeMkDate 2018 11 7
      equal expected result

    test "match nearest jul 4" do
      let matchJul4 = match true dateToMapStr "jul 4"
      let result = firstMatchingDate $ matchJul4 <$> dates
      let expected = Just $ matchJul4 $ unsafeMkDate 2019 7 4
      equal expected result

    test "match nearest jul 4 2018" do
      let matchJul4_2018 = match true dateToMapStr "jul 4 2018"
      let result = firstMatchingDate $ matchJul4_2018 <$> dates
      let expected = Just $ matchJul4_2018 $ unsafeMkDate 2018 7 4
      equal expected result

    test "match nearest 12/25" do
      let match12_25 = match true dateToMapStr "12 25"
      let result = firstMatchingDate $ match12_25 <$> dates
      let expected = Just $ match12_25 $ unsafeMkDate 2018 12 25
      equal expected result

    test "match 12/25/19" do
      let match12_25_19 = match true dateToMapStr "12 25 19"
      let result = firstMatchingDate $ match12_25_19 <$> dates
      let expected = Just $ match12_25_19 $ unsafeMkDate 2019 12 25
      equal expected result

    test "match 12/25/17" do
      let match12_25_17 = match true dateToMapStr "12 25 17"
      let result = firstMatchingDate $ match12_25_17 <$> dates
      let expected = Just $ match12_25_17 $ unsafeMkDate 2017 12 25
      equal expected result

  where
    toMapStr :: TestRecord -> Object String
    toMapStr (TR { name, value }) = fromFoldable [ Tuple "name" name, Tuple "value" value ]

    firstMatchingDate :: Array (Fuzzy Date) -> Maybe (Fuzzy Date)
    firstMatchingDate = find match'
      where
        match' (Fuzzy { ratio }) = ratio == 1 % 1

    dateToMapStr :: Date -> Object String
    dateToMapStr d =
      fromFoldable
        [ Tuple "mdy1" $ sYearMonth <> " " <> sDay <> " " <> sYear
        , Tuple "mdy2" $ sMonth <> " " <> sDay <> " " <> sYear
        , Tuple "weekday" $ sWeekDay
        , Tuple "wmdy1" $ sWeekDay <> " " <> sYearMonth <> " " <> sDay <> " " <> sYear
        , Tuple "ymd" $ sYear <> " " <> sMonth <> " " <> sDay
        ]
        where
          sYear = show $ fromEnum $ year d
          sMonth = show $ fromEnum $ month d
          sYearMonth = show $ month d
          sDay = show $ fromEnum $ day d
          sWeekDay = show $ weekday d

    dates :: Array Date
    dates = dateRange s e <> dateRange (prevYear s) (prevMonth s)
      where
        s = unsafeMkDate 2018 11 1
        e = unsafeMkDate 2020 10 31

    dateRange :: Date -> Date -> Array Date
    dateRange s e = go s e []
      where
        go start end acc
          | start == end = acc <> [start]
          | start >  end = reverse $ go end start acc
          | otherwise    = go (nextDay start) end (acc <> [start])

    nextDay :: Date -> Date
    nextDay = adjustDaysBy 1.0

    unsafeMkDate :: Int -> Int -> Int -> Date
    unsafeMkDate y m d = canonicalDate year month day
      where
        year  = unsafeMkYear y
        month = unsafeMkMonth m
        day   = unsafeMkDay d

    unsafeMkYear :: Int -> Year
    unsafeMkYear = unsafePartial fromJust <<< toEnum

    unsafeMkMonth :: Int -> Month
    unsafeMkMonth = unsafePartial fromJust <<< toEnum

    unsafeMkDay :: Int -> Day
    unsafeMkDay = unsafePartial fromJust <<< toEnum

    nextYear :: Date -> Date
    nextYear d = canonicalDate (unsafeSucc (year d)) (month d) (day d)

    prevYear :: Date -> Date
    prevYear d = canonicalDate (unsafePred (year d)) (month d) (day d)

    unsafeSucc :: ∀ a. Enum a => a -> a
    unsafeSucc = unsafePartial fromJust <<< succ

    unsafePred :: ∀ a. Enum a => a -> a
    unsafePred = unsafePartial fromJust <<< pred

    nextMonth :: Date -> Date
    nextMonth d = case (month d) of
      December -> canonicalDate (unsafeSucc (year d)) January (day d)
      other    -> canonicalDate (year d) (unsafeSucc (month d)) (day d)

    prevMonth :: Date -> Date
    prevMonth d = case (month d) of
      January -> canonicalDate (unsafePred (year d)) December (day d)
      other   -> canonicalDate (year d) (unsafePred (month d)) (day d)

    adjustDaysBy :: Number -> Date -> Date
    adjustDaysBy n = unsafePartial fromJust <<< next n
      where
        next :: Number -> Date -> Maybe Date
        next dur d = date <$> (adjust (Days dur) (toDateTime $ fromDate d))

newtype Distance' = Distance' Distance

derive instance newtypeDistance' :: Newtype Distance' _

derive newtype instance eqDistance' :: Eq Distance'
derive newtype instance ordDistance' :: Ord Distance'
derive newtype instance semigroupDistance' :: Semigroup Distance'
derive newtype instance monoidDistance' :: Monoid Distance'

instance arbitraryDistance' :: Arbitrary Distance' where
  arbitrary = map Distance' do
    Distance
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary

-- | - Associativity: `(x <> y) <> z = x <> (y <> z)`
checkSemigroup :: TestSuite
checkSemigroup = do
  test "Associativty law for Semigroup" do
    quickCheck' 1000 associativity
  where
  associativity :: Distance' -> Distance' -> Distance' -> Boolean
  associativity x y z = ((x <> y) <> z) == (x <> (y <> z))

-- | - Left identity: `mempty <> x = x`
-- | - Right identity: `x <> mempty = x`
checkMonoid :: TestSuite
checkMonoid = do
  test "Left identity law for Monoid" do
    quickCheck' 1000 leftIdentity

  test "Right identity law for Monoid" do
    quickCheck' 1000 rightIdentity

  where
  leftIdentity :: Distance' -> Boolean
  leftIdentity x = mempty <> x == x

  rightIdentity :: Distance' -> Boolean
  rightIdentity x = x <> mempty == x
