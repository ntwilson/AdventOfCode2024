module Day2Puzzle2 where

import Prelude

import Data.Array ((..))
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

  safeReports = pairedReports # Array.filter (Array.any reportIsSafe)

  pairedReports :: Array (Array (Array (Tuple Int Int)))
  pairedReports =
    allPossibleReports # map (map pairwise)

  pairwise x = Array.zip x (Array.drop 1 x)

  allPossibleReports :: Array (Array (Array Int))
  allPossibleReports = 
    reports <#> possibleReportPermutations

  possibleReportPermutations :: Array Int -> Array (Array Int)
  possibleReportPermutations report =
    Array.cons report $ modifiedReports report

  modifiedReports :: Array Int -> Array (Array Int)
  modifiedReports report =
    (1 .. Array.length report) # Array.mapMaybe (\i -> Array.deleteAt (i - 1) report)

  reportIsSafe reportPairs =
    (reportPairs # Array.all (\(a /\ b) -> let diff = abs (a - b) in 0 < diff && diff <= 3))
      &&
        ((reportPairs # Array.groupAllBy (comparing (\(a /\ b) -> sign (Int.toNumber a - Int.toNumber b))) # Array.length) == 1)



run :: Aff Unit
run = do
  contents <- getInputs
  let
    ans = solve contents

  log ("Day 2, Puzzle 2: " <> show ans)

