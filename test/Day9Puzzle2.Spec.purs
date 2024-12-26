module Day9Puzzle2.Spec where

import Prelude

import Data.Maybe (Maybe(..))
import Day9Puzzle2 (parseInput, processMemoryMap, rearrange, solve, windowed)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

spec :: Spec Unit
spec = do
  describe "Day9Puzzle2" do

    let
      testInput = "2333133121414131402"

      parsedInput =
        [ Just 0
        , Just 0
        , Nothing
        , Nothing
        , Nothing
        , Just 1
        , Just 1
        , Just 1
        , Nothing
        , Nothing
        , Nothing
        , Just 2
        , Nothing
        , Nothing
        , Nothing
        , Just 3
        , Just 3
        , Just 3
        , Nothing
        , Just 4
        , Just 4
        , Nothing
        , Just 5
        , Just 5
        , Just 5
        , Just 5
        , Nothing
        , Just 6
        , Just 6
        , Just 6
        , Just 6
        , Nothing
        , Just 7
        , Just 7
        , Just 7
        , Nothing
        , Just 8
        , Just 8
        , Just 8
        , Just 8
        , Just 9
        , Just 9
        ]

    it "can parse the test input" do
      processMemoryMap (parseInput testInput) `shouldEqual` parsedInput

    it "can rearrange the memory map" do
      rearrange parsedInput `shouldEqual`
        [ Just 0
        , Just 0
        , Just 9
        , Just 9
        , Just 2
        , Just 1
        , Just 1
        , Just 1
        , Just 7
        , Just 7
        , Just 7
        , Nothing
        , Just 4
        , Just 4
        , Nothing
        , Just 3
        , Just 3
        , Just 3
        , Nothing
        , Nothing
        , Nothing
        , Nothing
        , Just 5
        , Just 5
        , Just 5
        , Just 5
        , Nothing
        , Just 6
        , Just 6
        , Just 6
        , Just 6
        , Nothing
        , Nothing
        , Nothing
        , Nothing
        , Nothing
        , Just 8
        , Just 8
        , Just 8
        , Just 8
        , Nothing
        , Nothing
        ]

    it "can window an array by a given size" do
      windowed 3 [2,3,4,5,6] `shouldEqual` [[2,3,4], [3,4,5], [4,5,6]]

    it "can solve for the test input" do
      (parseInput testInput # processMemoryMap # solve) `shouldEqual` 2858.0

