module Day6Puzzle2.Spec where

import Prelude

import Data.Either (Either(..), isRight)
import Day6Puzzle2 (getInputs, parseInput, solve)
import Effect.Aff (Aff)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual, shouldSatisfy)

spec :: Spec Unit
spec = do
  describe "Day6Puzzle1" do

    it "can parse the real input" do
      parsedInput <- getInputs <#> parseInput :: Aff (Either String _)

      parsedInput `shouldSatisfy` isRight

    let
      testInput =
        [ "....#....."
        , ".........#"
        , ".........."
        , "..#......."
        , ".......#.."
        , ".........."
        , ".#..^....."
        , "........#."
        , "#........."
        , "......#..."
        ]

    it "can parse the test input" do
      (parseInput testInput :: Either String _) `shouldSatisfy` isRight

    it "can solve for the test input" do
      (parseInput testInput <#> solve :: Either String _) `shouldEqual` Right 6

