module Day4Puzzle2.Spec where

import Prelude

import Day4Puzzle2 (solve)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

spec :: Spec Unit
spec = do
  describe "Day4Puzzle2" do
    let
      testInput =
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

    it "should be able to solve the given test input" do
      solve testInput `shouldEqual` 9

