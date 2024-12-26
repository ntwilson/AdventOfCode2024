module Day9Puzzle1.Spec where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe(..))
import Day9Puzzle1 (parseInput, processMemoryMap, rearrange, solve)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

spec :: Spec Unit
spec = do
  describe "Day9Puzzle1" do

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
        ((Just <$> [0,0,9,9,8,1,1,1,8,8,8,2,7,7,7,3,3,3,6,4,4,6,5,5,5,5,6,6]) <> (Array.replicate 14 Nothing))

    it "can solve for the test input" do
      (parseInput testInput # processMemoryMap # solve) `shouldEqual` 1928.0

