module Data.Fuzzy where

import Prelude

import Data.Array (snoc, unsnoc)
import Data.Either (Either(..))
import Data.Foldable (all, foldl)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Ord (genericCompare)
import Data.Generic.Rep.Show (genericShow)
import Data.StrMap (StrMap, values)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Monoid (mempty)
import Data.Newtype (class Newtype, unwrap)
import Data.String (Pattern(..), drop, indexOf, lastIndexOf, take, toLower)
import Data.String.Utils (length, toCharArray, words)
import Data.Tuple (Tuple(..))

newtype Fuzzy a = Fuzzy
  { original :: a
  , result   :: StrMap (Maybe Result)
  , score    :: Rank
  }

derive instance genericFuzzy :: Generic (Fuzzy a) _
derive instance newtypeFuzzy :: Newtype (Fuzzy a) _
instance eqFuzzy :: Eq a => Eq (Fuzzy a) where eq = genericEq
instance showFuzzy :: Show a => Show (Fuzzy a) where show = genericShow
instance ordFuzzy :: Eq a => Ord (Fuzzy a) where
  compare (Fuzzy { score }) (Fuzzy { score: score' }) = compare score score'

newtype FuzzyStr = FuzzyStr
  { result :: Result
  , score  :: Rank
  }

derive instance genericFuzzyStr :: Generic FuzzyStr _
derive instance newtypeFuzzyStr :: Newtype FuzzyStr _
instance eqFuzzyStr :: Eq FuzzyStr where eq = genericEq
instance showFuzzyStr :: Show FuzzyStr where show = genericShow
instance ordFuzzyStr :: Ord FuzzyStr where
  compare (FuzzyStr { score }) (FuzzyStr { score: score' }) = compare score score'

data Depth = Full | Word | Char

derive instance genericDepth :: Generic Depth _
instance eqDepth :: Eq Depth where eq = genericEq
instance showDepth :: Show Depth where show = genericShow

data Pos = Start | Prefix | Mid | Suffix | End

derive instance genericPos :: Generic Pos _
instance eqPos :: Eq Pos where eq = genericEq
instance showPos :: Show Pos where show = genericShow

data Rank = Rank Int Int Int Int Int Int | None

derive instance genericRank :: Generic Rank _
instance eqRank :: Eq Rank where eq = genericEq
instance showRank :: Show Rank where show = genericShow
instance ordRank :: Ord Rank where compare = genericCompare

instance semiringRank :: Semiring Rank where
  add None r = r
  add r None = r
  add (Rank u v w x y z) (Rank u' v' w' x' y' z') =
    Rank (u + u') (v + v') (w + w') (x + x') (y + y') (z + z')
  zero = None
  mul None _ = None
  mul _ None = None
  mul (Rank u v w x y z) (Rank u' v' w' x' y' z') =
    Rank (u * u') (v * v') (w * w') (x * x') (y * y') (z * z')
  one = Rank 1 1 1 1 1 1

type MatchStrAcc =
  { substr :: String
  , pos    :: Pos
  , fuzzy  :: FuzzyStr
  }

type Result = Array (Either String String)

match :: ∀ a. Boolean -> (a -> StrMap String) -> String -> a -> Maybe (Fuzzy a)
match _ extract "" x =
  Just $ Fuzzy
    { original: x
    , result: (pure <<< pure <<< Left) <$> extract x
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
    matches :: StrMap (Maybe FuzzyStr)
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
  after $ foldl (matchStr' Full) initialAcc [ pattern ]
  where
    initialAcc :: MatchStrAcc
    initialAcc =
      { substr: str
      , pos: Start
      , fuzzy: FuzzyStr
        { result: mempty
        , score: None
        }
      }

    after :: MatchStrAcc -> Maybe FuzzyStr
    after { fuzzy: FuzzyStr { score: None } } = Nothing
    after { substr, pos, fuzzy: FuzzyStr { result, score: score@(Rank s _ _ _ _ _) } } =
      -- if there are no matches, this will evaluate to 2
      if s - (length pattern) == 2
         then Nothing
         else Just $ FuzzyStr { result: nextResult, score: nextScore }
        where
          nextResult :: Result
          nextResult = case substr of
            "" -> result
            _  -> snoc result (Left substr)

          nextScore :: Rank
          nextScore = score + (scoreDistance End $ length substr) + (scoreWord End 0 substr)

    matchStr' :: Depth -> MatchStrAcc -> String -> MatchStrAcc
    matchStr' depth { substr, pos, fuzzy: FuzzyStr { result, score } } pat =
      case indexOf' pat' substr' of
        Just distance ->
          { substr: drop (distance + (length pat)) substr
          , pos: Mid
          , fuzzy: FuzzyStr
            { result: nextResult distance
            , score: nextScore distance
            }
          }
        Nothing ->
          case depth of
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

          nextScore :: Int -> Rank
          nextScore d = case Tuple depth pos of
            Tuple Word Mid -> score + (scoreDistance Start d)
            _              -> score + (scoreDistance pos d) + (scoreWord pos d substr')

          nextAcc :: Pos -> MatchStrAcc
          nextAcc p =
            { substr
            , pos: p
            , fuzzy: FuzzyStr
              { result
              , score: scoreDepth score
              }
            }

scoreDepth :: Rank -> Rank
scoreDepth = (+) (Rank 1 0 0 0 0 0)

scoreDistance :: Pos -> Int -> Rank
scoreDistance pos d =
  m * (Rank d d d d d d)
  where
    m = case pos of
      Start  -> Rank 0 0 0 1 0 0
      Prefix -> Rank 0 0 1 0 0 0
      Mid    -> Rank 0 1 0 0 0 0
      Suffix -> Rank 0 0 0 0 1 0
      End    -> Rank 0 0 0 0 0 1

scoreWord :: Pos -> Int -> String -> Rank
scoreWord pos distance str =
  case pos of
    Start -> scoreDistance Prefix $ wordStart
    End   -> scoreDistance Suffix $ wordEnd
    _     -> None
    where
      before = take distance str
      wordStart = (length before) - ((fromMaybe (-1) $ lastIndexOf' " " before) + 1)
      after = length str
      wordEnd = fromMaybe (1 * after) (indexOf' " " str)

indexOf' :: String -> String -> Maybe Int
indexOf' = indexOf <<< Pattern

lastIndexOf' :: String -> String -> Maybe Int
lastIndexOf' = lastIndexOf <<< Pattern
