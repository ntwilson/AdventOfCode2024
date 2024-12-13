module Day6Puzzle1.Spec where

import Prelude

import Data.Either (Either(..), isRight)
import Day6Puzzle1 (getInputs, parseInput, solve)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual, shouldSatisfy)

spec :: Spec Unit
spec = do
  describe "Day6Puzzle1" do

    it "can parse the real input" do
      parsedInput <- getInputs <#> parseInput 

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
      parseInput testInput `shouldSatisfy` isRight

    it "can solve for the test input" do 
      (parseInput testInput >>= solve) `shouldEqual` Right 41

