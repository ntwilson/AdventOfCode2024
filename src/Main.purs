module Main where

import Prelude

import Data.Either (Either(..))
import Day10Puzzle1 as Day10Puzzle1
import Day10Puzzle2 as Day10Puzzle2
import Day11Puzzle1 as Day11Puzzle1
import Day11Puzzle2 as Day11Puzzle2
import Day12Puzzle1 as Day12Puzzle1
import Day13Puzzle1 as Day13Puzzle1
import Day13Puzzle2 as Day13Puzzle2
import Day14Puzzle1 as Day14Puzzle1
import Day14Puzzle2 as Day14Puzzle2
import Day1Puzzle1 as Day1Puzzle1
import Day1Puzzle2 as Day1Puzzle2
import Day2Puzzle1 as Day2Puzzle1
import Day2Puzzle2 as Day2Puzzle2
import Day3Puzzle1 as Day3Puzzle1
import Day3Puzzle2 as Day3Puzzle2
import Day4Puzzle1 as Day4Puzzle1
import Day4Puzzle2 as Day4Puzzle2
import Day5Puzzle1 as Day5Puzzle1
import Day5Puzzle2 as Day5Puzzle2
import Day6Puzzle1 as Day6Puzzle1
import Day7Puzzle1 as Day7Puzzle1
import Day8Puzzle1 as Day8Puzzle1
import Day8Puzzle2 as Day8Puzzle2
import Effect (Effect)
import Effect.Aff (Aff, runAff_)
import Effect.Class.Console (log)

runAll :: Aff Unit
runAll = do
  Day1Puzzle1.run
  Day1Puzzle2.run
  Day2Puzzle1.run
  Day2Puzzle2.run
  Day3Puzzle1.run
  Day3Puzzle2.run
  Day4Puzzle1.run
  Day4Puzzle2.run
  Day5Puzzle1.run
  Day5Puzzle2.run
  Day6Puzzle1.run
  -- skip because it takes a bit to run
  -- Day6Puzzle2.run 
  Day7Puzzle1.run
  -- skip because it takes a bit to run
  -- Day7Puzzle2.run
  Day8Puzzle1.run
  Day8Puzzle2.run
  -- skip because it takes a bit to run
  -- Day9Puzzle1.run
  -- Day9Puzzle2.run
  Day10Puzzle1.run
  Day10Puzzle2.run
  Day11Puzzle1.run
  Day11Puzzle2.run
  Day12Puzzle1.run
  -- skip because it takes a bit to run
  -- Day12Puzzle2.run
  Day13Puzzle1.run
  Day13Puzzle2.run
  Day14Puzzle1.run
  Day14Puzzle2.run

main :: Effect Unit
main = do
  runAff_ handler runAll

  where
  handler (Left err) = log $ "Error: " <> show err
  handler (Right _) = pure unit

