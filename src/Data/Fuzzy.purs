-- | The Fuzzy module provides functions and metrics for discerning
-- | how well a given value matches a string.

module Data.Fuzzy
  ( Segments
  , Distance(..)
  , FuzzyStr(..)
  , Fuzzy(..)
  , matchStr
  , match
  ) where

import Prelude

import Data.Array (snoc, unsnoc)
import Data.Either (Either(..), either)
import Data.Foldable (foldl)
import Data.Function (on)
import Data.Generic.Rep (class Generic)
import Data.Eq.Generic (genericEq)
import Data.Ord.Generic (genericCompare)
import Data.Show.Generic (genericShow)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (class Newtype, unwrap)
import Data.Rational (Rational, (%))
import Data.String (Pattern(..), Replacement(..), drop, indexOf, lastIndexOf, length, singleton, take, toCodePointArray, toLower)
import Data.String.Common (replaceAll)
import Data.String.Regex (parseFlags, regex, split)
import Data.Tuple (Tuple(..))
import Foreign.Object (Object, values)

-- | Type representing segments of matched and unmatched substrings.
-- | For example, when matching the pattern `"foo bar"` against the value
-- | `"food barn"`, the resulting `Segments` would be:
-- |
-- | ```[ Right "foo", Left "d ", Right "bar", Left "n" ]```

type Segments = Array (Either String String)

-- | Data representing the distance of a value from the pattern string.
-- | There are six magnitudes of distance, ranging from severe on the left,
-- | to almost trivial on the right. The smaller the distance, the better
-- | the match, with `Distance 0 0 0 0 0 0` representing a perfect match.
-- |
-- | `Distance` is an instance of `Ord`, allowing you to sort based on best distance.
-- |
-- | Each position's penalty is incremented for the following conditions:
-- |
-- | - *`0`: Failed matches.* This is the most severe penalty, which is added each time
-- |   the `match` function cannot match any of the following in order: The exact
-- |   pattern in its entirety, entire words within the pattern, individual
-- |   chars in unmatched words
-- | - *`1`: Between matches in a word.* This is the next most severe penalty,
-- |   added for each irrelevant char inbetween matched chars in each word. E.g.,
-- |   when matching the pattern "foo bar" against the value "f*l*ooded b*e*a*me*r",
-- |   this penalty would be set to `4`
-- | - *`2`: Matched word prefix.* This penalty is added whenever the first char for a
-- |   word in a pattern is not also the first char for the word its matched in. E.g.,
-- |   when matching the pattern "foo bar" against the value "an *un*followed *tu*bular",
-- |   this penalty would be set to `4`
-- | - *`3`: Chars till first match.* This penalty is added for every char up to the
-- |   first match. E.g., when matching the pattern "foo bar" against the value
-- |   "*an un*followed tubular", this penalty would be set to `5`
-- | - *`4`: Matched word suffix.* This penalty is the same as the prefix penalty but
-- |   for suffixes. E.g., when matching the pattern "foo bar" against the value
-- |   "foo*led* bar*n* hens", this penalty would be set to `4`
-- | - *`5`: Chars after last match.* This penalty is added for every char after the
-- |   last match. E.g., when matching the pattern "foo bar" against the value
-- |   "fooled bar*n hens*", this penalty would be set to `6`

data Distance = Distance Int Int Int Int Int Int | None

derive instance genericDistance :: Generic Distance _
instance eqDistance :: Eq Distance where eq = genericEq
instance showDistance :: Show Distance where show = genericShow
instance ordDistance :: Ord Distance where compare = genericCompare

instance semigroupDistance :: Semigroup Distance where
  append None d = d
  append d None = d
  append (Distance u v w x y z) (Distance u' v' w' x' y' z') =
    Distance (u + u') (v + v') (w + w') (x + x') (y + y') (z + z')

instance monoidDistance :: Monoid Distance where
  mempty = None

-- | Data representing the result of matching a string value against a string pattern
-- |
-- | Fields:
-- |
-- | - `original`: the original string value provided to `matchStr`
-- | - `segments`: `Segments` value, which will contain `original` split
-- |   into substrings of matched and unmatched chars (see `Segments` definition)
-- | - `distance`: `Distance` score from pattern to `original` (see `Distance` definition)
-- | - `ratio`: `Rational` representing percentage of matched chars. If all chars in pattern
-- |   are present, value will be a perfect `1 % 1`. If no chars are matched, value will be
-- |   `0 % 1`. A few other scenarios for the pattern "foo bar":
-- |   - "goo bar": `5 % 6`
-- |   - "go bar": `2 % 3`
-- |   - "bar": `1 % 2`
-- |   - "car": `1 % 3`
-- |   - "curry": `1 % 6`
-- |   This allows you to filter out results that are below a desired threshold

newtype FuzzyStr = FuzzyStr
  { original :: String
  , segments :: Segments
  , distance :: Distance
  , ratio    :: Rational
  }

derive instance genericFuzzyStr :: Generic FuzzyStr _
derive instance newtypeFuzzyStr :: Newtype FuzzyStr _
instance eqFuzzyStr :: Eq FuzzyStr where eq = genericEq
instance showFuzzyStr :: Show FuzzyStr where show = genericShow
instance ordFuzzyStr :: Ord FuzzyStr where
  compare = compare `on` (_.distance <<< unwrap)

-- | Data representing the result of matching any polymorphic value to a string pattern
-- |
-- | Fields:
-- |
-- | - `original`: the original polymorphic value provided to `match`
-- | - `segments`: `Object` of keys to `Segments` values, `Segments` values each
-- |   consisting of the original string values for each key, split into substrings
-- |   of matched and unmatched chars (see `Segments` or `match` definitions for more info)
-- | - `distance`: the best `Distance` score found for any provided key values (see `Distance`
-- |   and `match` defintions for more info)
-- | - `ratio`: the best ratio found for any provided key values (see `FuzzyStr` definition)

newtype Fuzzy a = Fuzzy
  { original :: a
  , segments  :: Object Segments
  , distance :: Distance
  , ratio    :: Rational
  }

derive instance genericFuzzy :: Generic (Fuzzy a) _
derive instance newtypeFuzzy :: Newtype (Fuzzy a) _
instance eqFuzzy :: Eq a => Eq (Fuzzy a) where eq = genericEq
instance showFuzzy :: Show a => Show (Fuzzy a) where show = genericShow
instance ordFuzzy :: Eq a => Ord (Fuzzy a) where
  compare = compare `on` (_.distance <<< unwrap)

-- Private data types

data Scope = Full | Word | Char

derive instance genericScope :: Generic Scope _
instance eqScope :: Eq Scope where eq = genericEq
instance showScope :: Show Scope where show = genericShow

data Pos = Start | Prefix | Mid | Suffix | End

derive instance genericPos :: Generic Pos _
instance eqPos :: Eq Pos where eq = genericEq
instance showPos :: Show Pos where show = genericShow

type MatchStrAcc =
  { substr :: String
  , pos    :: Pos
  , fuzzy  :: FuzzyStr
  }

-- | Function to match a string value against a string pattern
-- |
-- | Arguments:
-- |
-- | - `ignoreCase`: flag for whether or not uppercase and lowercase values should be
-- |   considered the same or not
-- | - `pattern`: string of chars you wish to match for in `str`
-- | - `str`: string to search through for the chars in `pattern`
-- |
-- | Returns:
-- |
-- | `FuzzyStr` data type with properties useful for highlighting matches, sorting
-- | a list of values, filtering out poor matches, etc.
-- |
-- | Examples:
-- |
-- | ```
-- | > matchStr true "foo bar" "fiz baz foo bar buz"
-- | FuzzyStr
-- |   { original: "fiz baz foo bar buz"
-- |   , segmemnts: [ Left "fiz baz ", Right "foo bar", Left " buz" ]
-- |   , distance: Distance 0 0 0 8 0 4
-- |   , ratio: 1 % 1
-- |   }
-- |
-- | See `test/Main.purs` for more examples

matchStr :: Boolean -> String -> String -> FuzzyStr
matchStr _ "" str =
  FuzzyStr
    { original: str
    , segments: [ Left str ]
    , distance: None
    , ratio: 1 % 1
    }
matchStr ignoreCase pattern str =
  after $ foldl (matchStr' Full) initialAcc [ pattern ]
  where
    initialAcc :: MatchStrAcc
    initialAcc =
      { substr: str
      , pos: Start
      , fuzzy: FuzzyStr
        { original: str
        , segments: mempty
        , distance: mempty
        , ratio: 1 % 1
        }
      }

    chars :: Int
    chars = length $ replaceAll (Pattern " ") (Replacement "") pattern

    matchStr' :: Scope -> MatchStrAcc -> String -> MatchStrAcc
    matchStr'
      scope
      { substr
      , pos
      , fuzzy: FuzzyStr { original, segments, distance, ratio }
      }
      pat =
      case indexOf' pat' substr' of
        Just spacing ->
          { substr: drop (spacing + (length pat)) substr
          , pos: Mid
          , fuzzy: FuzzyStr
            { original
            , segments: nextSegment spacing
            , distance: nextDistance spacing
            , ratio
            }
          }
        Nothing ->
          case scope of
            Full -> foldl (matchStr' Word) (nextAcc Start) $ words pat
            Word -> foldl (matchStr' Char) (nextAcc Start) $ (map singleton <<< toCodePointArray) pat
            Char -> nextAcc pos
        where
          Tuple pat' substr' =
            case ignoreCase of
              true -> Tuple (toLower pat) (toLower substr)
              _    -> Tuple pat substr

          nextSegment :: Int -> Segments
          nextSegment d = case Tuple d (unsnoc segments) of
            Tuple 0 (Just { init, last }) -> snoc init (last <> nextRight d)
            _                             -> segments <> nextLeft d <> [ nextRight d ]

          nextLeft :: Int -> Segments
          nextLeft d = case d of
            0 -> mempty
            _ -> [ Left $ take d substr ]

          nextRight :: Int -> Either String String
          nextRight d = Right $ take (length pat) (drop d substr)

          nextDistance :: Int -> Distance
          nextDistance d = case Tuple scope pos of
            Tuple Word Mid -> distance <> (scoreDistance Start d)
            _              -> distance <> (scoreDistance pos d) <> (scoreWord pos d substr')

          nextAcc :: Pos -> MatchStrAcc
          nextAcc p =
            { substr
            , pos: p
            , fuzzy: FuzzyStr
              { original
              , segments
              , distance: scoreScope distance
              , ratio: if scope == Char then ratio - (1 % chars) else ratio
              }
            }

    after :: MatchStrAcc -> FuzzyStr
    after { substr, pos, fuzzy: FuzzyStr { original, segments, distance, ratio } } =
      FuzzyStr { original, segments: nextSegment, distance: nextDistance, ratio }
        where
          nextSegment :: Segments
          nextSegment = case substr of
            "" -> segments
            _  -> snoc segments (Left substr)

          nextDistance :: Distance
          nextDistance | ratio == (0 % 1) = distance
                       | otherwise        = distance
                                            <> (scoreDistance End $ length substr)
                                            <> (scoreWord End 0 substr)

-- | Funtion to match any polymorhic value against a string pattern
-- |
-- | Arguments:
-- |
-- | - `ignoreCase`: flag for whether or not uppercase and lowercase values should be
-- |   considered the same or not
-- | - `extract`: function from your polymorphic `val` to `Object String` so `match`
-- |   can search through each string for the desired `pattern`
-- | - `pattern`: string of chars you wish to match for in any of the strings `extract`
-- |   pulls from the provided `val`
-- | - `val`: polymorphic value that `match` will pull out strings from via the provided
-- |   `extract` function and then search through for the chars in `pattern`
-- |
-- | Returns:
-- |
-- | `Fuzzy` data type with properties useful for highlighting matches, sorting a list of
-- | values, filtering out poor matches, etc.
-- |
-- | Examples:
-- |
-- | ```
-- | > toMapStr { name, value } = fromFoldable [ Tuple "name" name, Tuple "value" value ]
-- | > match true toMapStr "foo bar" { name: "Foo Bar Baz", value: "foobar" }
-- | Fuzzy
-- |   { original: { name: "Foo Bar Baz", value: "foobar" }
-- |   , segments: fromFoldable [ Tuple "name" [ Right "Foo Bar", Left " Baz" ]
-- |                            , Tuple "value" [ Right "foobar" ] ]
-- |   , distance: Distance 0 0 0 0 0 4
-- |   , ratio: 1 % 1
-- |   }
-- |
-- | See `test/Main.purs` for more examples

match :: ∀ a. Boolean -> (a -> Object String) -> String -> a -> Fuzzy a
match _ extract "" val =
  Fuzzy
    { original: val
    , segments: (pure <<< Left) <$> extract val
    , distance: mempty
    , ratio: 1 % 1
    }
match ignoreCase extract pattern val =
  Fuzzy
    { original: val
    , segments: (_.segments <<< unwrap) <$> matches
    , distance: foldl minDistance mempty $ fuzzies
    , ratio: foldl maxRatio (0 % 1) $ fuzzies
    }
  where
    matches :: Object FuzzyStr
    matches = matchStr ignoreCase pattern <$> extract val

    fuzzies :: Array FuzzyStr
    fuzzies = values matches

    minDistance :: Distance -> FuzzyStr -> Distance
    minDistance d (FuzzyStr { distance }) = min d distance

    maxRatio :: Rational -> FuzzyStr -> Rational
    maxRatio r (FuzzyStr { ratio }) = max r ratio

-- Private functions

scoreScope :: Distance -> Distance
scoreScope = (<>) (Distance 1 0 0 0 0 0)

scoreDistance :: Pos -> Int -> Distance
scoreDistance Start  d = Distance 0 0 0 d 0 0
scoreDistance Prefix d = Distance 0 0 d 0 0 0
scoreDistance Mid    d = Distance 0 d 0 0 0 0
scoreDistance Suffix d = Distance 0 0 0 0 d 0
scoreDistance End    d = Distance 0 0 0 0 0 d

scoreWord :: Pos -> Int -> String -> Distance
scoreWord pos distance str =
  case pos of
    Start -> scoreDistance Prefix $ wordStart
    End   -> scoreDistance Suffix $ wordEnd
    _     -> mempty
    where
      before = take distance str
      wordStart = (length before) - ((fromMaybe (-1) $ lastIndexOf' " " before) + 1)
      after = length str
      wordEnd = fromMaybe (1 * after) (indexOf' " " str)

indexOf' :: String -> String -> Maybe Int
indexOf' = indexOf <<< Pattern

lastIndexOf' :: String -> String -> Maybe Int
lastIndexOf' = lastIndexOf <<< Pattern

-- Split strings on whitespace using code points
words :: String -> Array String
words = either (const <<< pure) split regex'
  where
    regex' = regex "\\s+" $ parseFlags "g"
