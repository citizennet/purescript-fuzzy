module Data.Fuzzy
  ( Fuzzy(..)
  , FuzzyStr(..)
  , Distance(..)
  , Result
  , match
  , matchStr
  ) where

import Prelude

import Data.Array (snoc, unsnoc)
import Data.Either (Either(..))
import Data.Foldable (foldl)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Ord (genericCompare)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Monoid (class Monoid, mempty)
import Data.Newtype (class Newtype, unwrap)
import Data.Rational (Rational, (%))
import Data.StrMap (StrMap, values)
import Data.String (Pattern(..), drop, indexOf, lastIndexOf, take, toLower)
import Data.String.Utils (length, replaceAll, toCharArray, words)
import Data.Tuple (Tuple(..))
import Test.QuickCheck.Arbitrary (class Arbitrary, arbitrary)

newtype Fuzzy a = Fuzzy
  { original :: a
  , result   :: StrMap Result
  , distance :: Distance
  , ratio    :: Rational
  }

derive instance genericFuzzy :: Generic (Fuzzy a) _
derive instance newtypeFuzzy :: Newtype (Fuzzy a) _
instance eqFuzzy :: Eq a => Eq (Fuzzy a) where eq = genericEq
instance showFuzzy :: Show a => Show (Fuzzy a) where show = genericShow
instance ordFuzzy :: Eq a => Ord (Fuzzy a) where
  compare (Fuzzy { distance }) (Fuzzy { distance: distance' }) = compare distance distance'

newtype FuzzyStr = FuzzyStr
  { original :: String
  , result   :: Result
  , distance :: Distance
  , ratio    :: Rational
  }

derive instance genericFuzzyStr :: Generic FuzzyStr _
derive instance newtypeFuzzyStr :: Newtype FuzzyStr _
instance eqFuzzyStr :: Eq FuzzyStr where eq = genericEq
instance showFuzzyStr :: Show FuzzyStr where show = genericShow
instance ordFuzzyStr :: Ord FuzzyStr where
  compare (FuzzyStr { distance }) (FuzzyStr { distance: distance' }) = compare distance distance'

data Scope = Full | Word | Char

derive instance genericScope :: Generic Scope _
instance eqScope :: Eq Scope where eq = genericEq
instance showScope :: Show Scope where show = genericShow

data Pos = Start | Prefix | Mid | Suffix | End

derive instance genericPos :: Generic Pos _
instance eqPos :: Eq Pos where eq = genericEq
instance showPos :: Show Pos where show = genericShow

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

instance arbitraryDistance :: Arbitrary Distance where
  arbitrary = Distance
    <$> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary

type MatchStrAcc =
  { substr :: String
  , pos    :: Pos
  , fuzzy  :: FuzzyStr
  }

type Result = Array (Either String String)

match :: ∀ a. Boolean -> (a -> StrMap String) -> String -> a -> Fuzzy a
match _ extract "" x =
  Fuzzy
    { original: x
    , result: (pure <<< Left) <$> extract x
    , distance: mempty
    , ratio: 1 % 1
    }
match ignoreCase extract pattern x =
  Fuzzy
    { original: x
    , result: (_.result <<< unwrap) <$> matches
    , distance: foldl minDistance mempty $ fuzzies
    , ratio: foldl maxRatio (0 % 1) $ fuzzies
    }
  where
    matches :: StrMap FuzzyStr
    matches = matchStr ignoreCase pattern <$> extract x

    fuzzies :: Array FuzzyStr
    fuzzies = values matches

    minDistance :: Distance -> FuzzyStr -> Distance
    minDistance d (FuzzyStr { distance }) = min d distance

    maxRatio :: Rational -> FuzzyStr -> Rational
    maxRatio r (FuzzyStr { ratio }) = max r ratio

matchStr :: Boolean -> String -> String -> FuzzyStr
matchStr _ "" str =
  FuzzyStr
    { original: str
    , result: [ Left str ]
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
        , result: mempty
        , distance: mempty
        , ratio: 1 % 1
        }
      }

    chars :: Int
    chars = length $ replaceAll " " "" pattern

    matchStr' :: Scope -> MatchStrAcc -> String -> MatchStrAcc
    matchStr'
      scope
      { substr
      , pos
      , fuzzy: FuzzyStr { original, result, distance, ratio }
      }
      pat =
      case indexOf' pat' substr' of
        Just spacing ->
          { substr: drop (spacing + (length pat)) substr
          , pos: Mid
          , fuzzy: FuzzyStr
            { original
            , result: nextResult spacing
            , distance: nextDistance spacing
            , ratio
            }
          }
        Nothing ->
          case scope of
            Full -> foldl (matchStr' Word) (nextAcc Start) $ words pat
            Word -> foldl (matchStr' Char) (nextAcc Start) $ toCharArray pat
            Char -> nextAcc pos
        where
          Tuple pat' substr' =
            case ignoreCase of
              true -> Tuple (toLower pat) (toLower substr)
              _    -> Tuple pat substr

          nextResult :: Int -> Result
          nextResult d = case Tuple d (unsnoc result) of
            Tuple 0 (Just { init, last }) -> snoc init (last <> nextRight d)
            _                             -> result <> nextLeft d <> [ nextRight d ]

          nextLeft :: Int -> Result
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
              , result
              , distance: scoreScope distance
              , ratio: if scope == Char then ratio - (1 % chars) else ratio
              }
            }

    after :: MatchStrAcc -> FuzzyStr
    after { substr, pos, fuzzy: FuzzyStr { original, result, distance, ratio } } =
      FuzzyStr { original, result: nextResult, distance: nextDistance, ratio }
        where
          nextResult :: Result
          nextResult = case substr of
            "" -> result
            _  -> snoc result (Left substr)

          nextDistance :: Distance
          nextDistance | ratio == (0 % 1) = distance
                       | otherwise        = distance
                                            <> (scoreDistance End $ length substr)
                                            <> (scoreWord End 0 substr)

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
