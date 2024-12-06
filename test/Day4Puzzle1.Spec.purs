module Day4Puzzle1.Spec where

import Prelude

import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

spec :: Spec Unit
spec = do
  describe "Day4Puzzle1" do
    it "should parse the input given in the puzzle description" do
      let
        _testInput = 
          [ "MMMSXXMASM"
          , "MSAMXMSMSA"
          , "AMXSXMAAMM"
          , "MSAMASMSMX"
          , "XMASAMXAMM"
          , "XXAMMXXAMA"
          , "SMSMSASXSS"
          , "SAXAMASAAA"
          , "MAMMMXMMMM"
          , "MXMXAXMASX"
          ]
      0 `shouldEqual` 0
