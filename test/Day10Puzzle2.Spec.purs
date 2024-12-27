module Day10Puzzle2.Spec where

import Prelude

import Day10Puzzle2 (parseInput, solve)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

spec :: Spec Unit
spec = do
  describe "Day10Puzzle2" do

    let
      testInput =
        [ "89010123"
        , "78121874"
        , "87430965"
        , "96549874"
        , "45678903"
        , "32019012"
        , "01329801"
        , "10456732"
        ]

      parsedInput =
        [ [ { row: 0, col: 0, height: 8 }, { row: 0, col: 1, height: 9 }, { row: 0, col: 2, height: 0 }, { row: 0, col: 3, height: 1 }, { row: 0, col: 4, height: 0 }, { row: 0, col: 5, height: 1 }, { row: 0, col: 6, height: 2 }, { row: 0, col: 7, height: 3 } ]
        , [ { row: 1, col: 0, height: 7 }, { row: 1, col: 1, height: 8 }, { row: 1, col: 2, height: 1 }, { row: 1, col: 3, height: 2 }, { row: 1, col: 4, height: 1 }, { row: 1, col: 5, height: 8 }, { row: 1, col: 6, height: 7 }, { row: 1, col: 7, height: 4 } ]
        , [ { row: 2, col: 0, height: 8 }, { row: 2, col: 1, height: 7 }, { row: 2, col: 2, height: 4 }, { row: 2, col: 3, height: 3 }, { row: 2, col: 4, height: 0 }, { row: 2, col: 5, height: 9 }, { row: 2, col: 6, height: 6 }, { row: 2, col: 7, height: 5 } ]
        , [ { row: 3, col: 0, height: 9 }, { row: 3, col: 1, height: 6 }, { row: 3, col: 2, height: 5 }, { row: 3, col: 3, height: 4 }, { row: 3, col: 4, height: 9 }, { row: 3, col: 5, height: 8 }, { row: 3, col: 6, height: 7 }, { row: 3, col: 7, height: 4 } ]
        , [ { row: 4, col: 0, height: 4 }, { row: 4, col: 1, height: 5 }, { row: 4, col: 2, height: 6 }, { row: 4, col: 3, height: 7 }, { row: 4, col: 4, height: 8 }, { row: 4, col: 5, height: 9 }, { row: 4, col: 6, height: 0 }, { row: 4, col: 7, height: 3 } ]
        , [ { row: 5, col: 0, height: 3 }, { row: 5, col: 1, height: 2 }, { row: 5, col: 2, height: 0 }, { row: 5, col: 3, height: 1 }, { row: 5, col: 4, height: 9 }, { row: 5, col: 5, height: 0 }, { row: 5, col: 6, height: 1 }, { row: 5, col: 7, height: 2 } ]
        , [ { row: 6, col: 0, height: 0 }, { row: 6, col: 1, height: 1 }, { row: 6, col: 2, height: 3 }, { row: 6, col: 3, height: 2 }, { row: 6, col: 4, height: 9 }, { row: 6, col: 5, height: 8 }, { row: 6, col: 6, height: 0 }, { row: 6, col: 7, height: 1 } ]
        , [ { row: 7, col: 0, height: 1 }, { row: 7, col: 1, height: 0 }, { row: 7, col: 2, height: 4 }, { row: 7, col: 3, height: 5 }, { row: 7, col: 4, height: 6 }, { row: 7, col: 5, height: 7 }, { row: 7, col: 6, height: 3 }, { row: 7, col: 7, height: 2 } ]
        ]

    it "can parse the test input" do
      parseInput testInput `shouldEqual` parsedInput

    it "can solve for the test input" do
      (parseInput testInput # solve) `shouldEqual` 81

