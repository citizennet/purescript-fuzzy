module Data.Fuzzy where

import Prelude

import Data.Array (snoc, unsnoc)
import Data.Either (Either(..), note)
import Data.Foldable (foldl)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Ord (genericCompare)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..))
import Data.Monoid (mempty)
import Data.String (Pattern(..), drop, indexOf, take, toLower)
import Data.String.Utils (charAt, length, toCharArray)
import Data.Tuple (Tuple(..))

data Fuzzy t =
  Fuzzy { original :: t
        , result   :: Array Result
        , score    :: Rank
        }

data FuzzBall =
  FuzzBall { substr    :: String
           , result :: Result
           , score  :: Rank
           , pos    :: Pos
           }

derive instance genericFuzzBall :: Generic FuzzBall _
instance eqFuzzBall :: Eq FuzzBall where eq = genericEq
instance showFuzzball :: Show FuzzBall where show = genericShow

data Pos = First | Mid | Last

derive instance genericPos :: Generic Pos _
instance eqPos :: Eq Pos where eq = genericEq
instance showPos :: Show Pos where show = genericShow

data Rank = Rank Int Int Int

derive instance genericRank :: Generic Rank _
instance eqRank :: Eq Rank where eq = genericEq
instance showRank :: Show Rank where show = genericShow
instance ordRank :: Ord Rank where compare = genericCompare
instance semiringRank :: Semiring Rank where
  add (Rank x y z) (Rank x' y' z') = Rank (x + x') (y + y') (z + z')
  zero = Rank 0 0 0
  mul (Rank x y z) (Rank x' y' z') = Rank (x * x') (y * y') (z * z')
  one = Rank 1 1 1

type Result = Array (Either String String)

{--match :: ∀ t. String -> t -> (t -> Array String) -> Boolean -> Fuzzy t--}
{--match pattern t extract caseSensitive =--}

match' :: Boolean -> String -> String -> Maybe FuzzBall
match' _          ""      str =
  Just $ FuzzBall { substr: str
                  , result: [ Left str ]
                  , score: scoreDistance Last (length str)
                  , pos: Last
                  }
match' ignoreCase pattern str =
  after $ foldl matchCur initial (toCharArray pattern)
  where
    initial :: Maybe FuzzBall
    initial = Just $ FuzzBall { substr: str, result: mempty, score: zero, pos: First }

    matchCur :: Maybe FuzzBall -> String -> Maybe FuzzBall
    matchCur Nothing _ = Nothing
    matchCur (Just (FuzzBall { substr, result, score, pos })) patChar =
      case indexOf (Pattern patChar') substr' of
           Nothing ->
             Nothing
           Just distance ->
             Just $ fuzzball distance
             where
               Tuple patChar' substr' =
                 case ignoreCase of
                   true -> Tuple (toLower patChar) (toLower substr)
                   _    -> Tuple patChar substr

               fuzzball :: Int -> FuzzBall
               fuzzball distance =
                 FuzzBall { substr: drop (distance + (length patChar)) substr
                          , result: if distance == 0 then mapResult else appendResult
                          , score: score + (scoreDistance pos distance)
                          , pos: Mid
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

    after :: Maybe FuzzBall -> Maybe FuzzBall
    after Nothing = Nothing
    after (Just (FuzzBall { substr, result, score })) =
      Just $ FuzzBall { substr: ""
                      , result: result <> if substr == "" then mempty else [ Left substr ]
                      , score: score + (scoreDistance Last $ length substr)
                      , pos: Last
                      }

scoreDistance :: Pos -> Int -> Rank
scoreDistance pos d =
  m * (Rank d d d)
  where
    m = case pos of
      First -> Rank 0 1 0
      Mid   -> Rank 1 0 0
      Last  -> Rank 0 0 1
