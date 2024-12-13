module Day4Puzzle1.Spec where

import Prelude

import Data.Foldable (sum)
import Day4Puzzle1 (backwards, diagonalDownForwards, solve, xmasInstances)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

spec :: Spec Unit
spec = do
  describe "Day4Puzzle1" do
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

    it "should be able to reverse all the strings" do
      backwards testInput `shouldEqual`
        [ "MSAMXXSMMM"
        , "ASMSMXMASM"
        , "MMAAMXSXMA"
        , "XMSMSAMASM"
        , "MMAXMASAMX"
        , "AMAXXMMAXX"
        , "SSXSASMSMS"
        , "AAASAMAXAS"
        , "MMMMXMMMAM"
        , "XSAMXAXMXM"
        ]

    it "should be able to find the down-forward diagonals" do
      diagonalDownForwards testInput `shouldEqual`
        [ "MSXMAXSAMX"
        , "MASAMXXAM"
        , "MMXSXASA"
        , "SXMMAMS"
        , "XMASMA"
        , "XSAMM"
        , "MMMX"
        -- , "ASM"
        -- , "SA"
        -- , "M"
        -- , "M"
        -- , "MX"
        -- , "SAM"
        , "SAMX"
        , "XMXMA"
        , "XXSAMX"
        , "MMAMMXM"
        , "ASAMSAMA"
        , "MMASMASMS"
        ]

    it "should be able to find all the xmas instances" do
       sum (xmasInstances <$> testInput) `shouldEqual` 3
       sum (xmasInstances <$> (diagonalDownForwards testInput)) `shouldEqual` 1

    it "should be able to solve the given test input" do
      solve testInput `shouldEqual` 18

