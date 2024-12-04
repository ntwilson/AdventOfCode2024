module Day1Puzzle1 where

import Prelude

import Data.Array as Array
import Data.Foldable (sum)
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Data.Ord (abs)
import Data.String (Pattern(..))
import Data.String as String
import Data.Tuple (Tuple)
import Data.Tuple.Nested ((/\))
import Effect.Aff (Aff)
import Effect.Class.Console (log)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile)

getInputs :: Aff (Array (Tuple Int Int))
getInputs = do
  contents <- readTextFile UTF8 "inputs/day1.txt"
  let 
    lines = String.split (Pattern "\n") contents
    inputValues = String.split (Pattern "   ") <$> lines
    inputPairs = inputValues 
      # Array.mapMaybe (case _ of 
        [x, y]
        | Just left <- Int.fromString x
        , Just right <- Int.fromString y -> Just (left /\ right)
        _ -> Nothing
      )
  pure inputPairs

solve :: Array (Tuple Int Int) -> _
solve contents =
  let 
    (lefts /\ rights) = Array.unzip contents
    sortedLefts = Array.sort lefts
    sortedRights = Array.sort rights
    diffs = Array.zipWith (\l r -> abs (l - r)) sortedLefts sortedRights

  in 
    sum diffs


run :: Aff Unit
run = do
  contents <- getInputs
  let 
    ans = solve contents
  
  log ("Day 1, Puzzle 1: " <> show ans)

