module Main where

import Prelude

import Data.Either (Either(..))
import Day1Puzzle1 as Day1Puzzle1
import Day1Puzzle2 as Day1Puzzle2
import Day2Puzzle1 as Day2Puzzle1
import Day2Puzzle2 as Day2Puzzle2
import Day3Puzzle1 as Day3Puzzle1
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

main :: Effect Unit
main = do
  runAff_ handler runAll

  where 
  handler (Left err) = log $ "Error: " <> show err
  handler (Right _) = pure unit


