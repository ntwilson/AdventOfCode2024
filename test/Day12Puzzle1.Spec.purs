module Day12Puzzle1.Spec where

import Prelude

import Data.Array as Array
import Data.String.CodeUnits as String
import Day12Puzzle1 (regionFor, regions, solve)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

spec :: Spec Unit
spec = do
  describe "Day12Puzzle1" do

    let
      testInput =
        [ [ 'A', 'A', 'A', 'A' ]
        , [ 'B', 'B', 'C', 'D' ]
        , [ 'B', 'B', 'C', 'C' ]
        , [ 'E', 'E', 'E', 'C' ]
        ]

    it "extracts a region" do
      regionFor testInput { row: 0, col: 0 } 'A' `shouldEqual` Array.sort [ { row: 0, col: 0 }, { row: 0, col: 1 }, { row: 0, col: 2 }, { row: 0, col: 3 } ]
      regionFor testInput { row: 1, col: 0 } 'B' `shouldEqual` Array.sort [ { row: 1, col: 0 }, { row: 1, col: 1 }, { row: 2, col: 0 }, { row: 2, col: 1 } ]
      regionFor testInput { row: 1, col: 2 } 'C' `shouldEqual` Array.sort [ { row: 1, col: 2 }, { row: 2, col: 2 }, { row: 2, col: 3 }, { row: 3, col: 3 } ]

    it "extracts all the regions of the input" do
      regions testInput `shouldEqual`
        ( Array.sort <$>
            [ [ { row: 0, col: 0 }, { row: 0, col: 1 }, { row: 0, col: 2 }, { row: 0, col: 3 } ]
            , [ { row: 1, col: 0 }, { row: 1, col: 1 }, { row: 2, col: 0 }, { row: 2, col: 1 } ]
            , [ { row: 1, col: 2 }, { row: 2, col: 2 }, { row: 2, col: 3 }, { row: 3, col: 3 } ]
            , [ { row: 1, col: 3 } ]
            , [ { row: 3, col: 0 }, { row: 3, col: 1 }, { row: 3, col: 2 } ]
            ]
        )

    it "solves for the test input" do
      solve testInput `shouldEqual` 140

    let 
      testInputWithInnerRegions = 
        [ "OOOOO"
        , "OXOXO"
        , "OOOOO"
        , "OXOXO"
        , "OOOOO"
        ]

    it "solves the case of inner regions" do
      solve (map String.toCharArray testInputWithInnerRegions) `shouldEqual` 772

    let
      biggerTestInput =
        [ "RRRRIICCFF"
        , "RRRRIICCCF"
        , "VVRRRCCFFF"
        , "VVRCCCJFFF"
        , "VVVVCJJCFE"
        , "VVIVCCJJEE"
        , "VVIIICJJEE"
        , "MIIIIIJJEE"
        , "MIIISIJEEE"
        , "MMMISSJEEE"
        ]

    it "solves for the bigger test input" do
      solve (map String.toCharArray biggerTestInput) `shouldEqual` 1930

