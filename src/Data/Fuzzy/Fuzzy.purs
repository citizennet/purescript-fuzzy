module Data.Fuzzy where

import Prelude

import Data.Array (snoc, unsnoc)
import Data.Either (Either(..), note)
import Data.Foldable (all, foldl)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Ord (genericCompare)
import Data.Generic.Rep.Show (genericShow)
import Data.Map (Map, empty, insert, toUnfoldable, values)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Monoid (mempty)
import Data.Newtype (class Newtype, unwrap)
import Data.String (Pattern(..), drop, indexOf, take, toLower)
import Data.String.Utils (charAt, length, toCharArray)
import Data.Tuple (Tuple(..))

newtype Fuzzy a = Fuzzy
  { original :: a
  , result   :: Map String (Maybe Result)
  , score    :: Rank
  }

derive instance genericFuzzy :: Generic (Fuzzy a) _

derive instance newtypeFuzzy :: Newtype (Fuzzy a) _

instance eqFuzzy :: Eq a => Eq (Fuzzy a) where eq = genericEq

instance showFuzzy :: Show a => Show (Fuzzy a) where show = genericShow

newtype FuzzyStr = FuzzyStr
  { result :: Result
  , score  :: Rank
  }

newtype FuzzyStr' = FuzzyStr'
  { substr :: String
  , result :: Result
  , score  :: Rank
  , pos    :: Pos
  }

derive instance genericFuzzyStr :: Generic FuzzyStr _

derive instance newtypeFuzzyStr :: Newtype FuzzyStr _

instance eqFuzzyStr :: Eq FuzzyStr where eq = genericEq

instance showFuzzyStr :: Show FuzzyStr where show = genericShow

data Pos = Before | Between | Behind | Rest

derive instance genericPos :: Generic Pos _

instance eqPos :: Eq Pos where eq = genericEq

instance showPos :: Show Pos where show = genericShow

data Rank = Rank Int Int Int Int | None

derive instance genericRank :: Generic Rank _

instance eqRank :: Eq Rank where eq = genericEq

instance showRank :: Show Rank where show = genericShow

instance ordRank :: Ord Rank where compare = genericCompare

instance semiringRank :: Semiring Rank where
  add None r = r
  add r None = r
  add (Rank w x y z) (Rank w' x' y' z') = Rank (w + w') (x + x') (y + y') (z + z')
  zero = None
  mul None _ = None
  mul _ None = None
  mul (Rank w x y z) (Rank w' x' y' z') = Rank (w * w') (x * x') (y * y') (z * z')
  one = Rank 1 1 1 1

type Result = Array (Either String String)

match :: ∀ a. Boolean -> (a -> Map String String) -> String -> a -> Maybe (Fuzzy a)
match _ extract "" x =
  Just $ Fuzzy
    { original: x
    , result: (\s -> Just [ Left s ]) <$> extract x
    , score: zero
    }
match ignoreCase extract pattern x =
  case all ((==) Nothing) matches of
    true -> Nothing
    _    -> Just $ Fuzzy
      { original: x
      , result: map (_.result <<< unwrap) <$> matches
      , score: foldl minScore None $ values matches
      }
  where
    matches :: Map String (Maybe FuzzyStr)
    matches = matchStr ignoreCase pattern <$> extract x

    minScore :: Rank -> Maybe FuzzyStr -> Rank
    minScore r Nothing = r
    minScore r (Just (FuzzyStr { score })) = min r score

matchStr :: Boolean -> String -> String -> Maybe FuzzyStr
matchStr _          ""      str =
  Just $ FuzzyStr
    { result: [ Left str ]
    , score: None
    }
matchStr ignoreCase pattern str =
  after $ foldl matchCur initial (toCharArray pattern)
  where
    initial :: Maybe FuzzyStr'
    initial = Just $ FuzzyStr' { substr: str, result: mempty, score: zero, pos: Before }

    matchCur :: Maybe FuzzyStr' -> String -> Maybe FuzzyStr'
    matchCur Nothing _ = Nothing
    matchCur (Just (FuzzyStr' { substr, result, score, pos })) patChar =
      case indexOf (Pattern patChar') substr' of
           Nothing ->
             Nothing
           Just distance ->
             Just $ fuzz distance
             where
               Tuple patChar' substr' =
                 case ignoreCase of
                   true -> Tuple (toLower patChar) (toLower substr)
                   _    -> Tuple patChar substr

               fuzz :: Int -> FuzzyStr'
               fuzz distance =
                 FuzzyStr'
                   { substr: drop (distance + (length patChar)) substr
                   , result: if distance == 0 then mapResult else appendResult
                   , score: score + (scoreDistance pos distance)
                   , pos: Between
                   }
                 where
                   mapResult :: Result
                   mapResult = case unsnoc result of
                     Nothing -> appendResult
                     Just { init, last } -> snoc init (last <> note "" (charAt distance substr))

                   appendResult :: Result
                   appendResult = result <> nextLeft <> nextRight

                   nextLeft :: Result
                   nextLeft = case distance of
                     0 -> mempty
                     _ -> [ Left $ take distance substr ]

                   nextRight :: Result
                   nextRight = [ note "" (charAt distance substr) ]

    after :: Maybe FuzzyStr' -> Maybe FuzzyStr
    after Nothing = Nothing
    after (Just (FuzzyStr' { substr, result, score })) =
      Just $ FuzzyStr { result: result <> if substr == "" then mempty else [ Left substr ]
                      , score: score + scoreBehind + scoreRest
                      }
        where
          lenRem = length substr
          scoreBehind = scoreDistance Behind (fromMaybe (1 * lenRem) $ indexOf (Pattern " ") substr)
          scoreRest = scoreDistance Rest lenRem

scoreDistance :: Pos -> Int -> Rank
scoreDistance pos d =
  m * (Rank d d d d)
  where
    m = case pos of
      Before  -> Rank 0 1 0 0
      Between -> Rank 1 0 0 0
      Behind  -> Rank 0 0 1 0
      Rest    -> Rank 0 0 0 1
