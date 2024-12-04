module Day2Puzzle1 where

import Prelude

import Data.Array as Array
import Data.Int as Int
import Data.Number (sign)
import Data.Ord (abs)
import Data.String (Pattern(..))
import Data.String as String
import Data.Tuple (Tuple)
import Data.Tuple.Nested ((/\))
import Effect.Aff (Aff)
import Effect.Class.Console (log)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile)

getInputs :: Aff (Array (Array Int))
getInputs = do
  contents <- readTextFile UTF8 "inputs/day2.txt"
  let
    lines = String.split (Pattern "\n") contents
    inputValues = Array.filter (not Array.null) $ String.split (Pattern " ") <$> lines
    inputPairs = inputValues <#> Array.mapMaybe Int.fromString
  pure inputPairs

solve :: Array (Array Int) -> _
solve reports =
  Array.length safeReports

  where

  safeReports = pairedReports # Array.filter reportIsSafe

  reportIsSafe reportPairs =
    (reportPairs # Array.all (\(a /\ b) -> 0 < abs (a - b) && abs (a - b) <= 3))
      &&
        ((reportPairs # Array.groupAllBy (comparing (\(a /\ b) -> sign (Int.toNumber a - Int.toNumber b))) # Array.length) == 1)

  pairedReports :: Array (Array (Tuple Int Int))
  pairedReports =
    reports <#> (\report -> Array.zip report (Array.drop 1 report))

run :: Aff Unit
run = do
  contents <- getInputs
  let
    ans = solve contents

  log ("Day 2, Puzzle 1: " <> show ans)

