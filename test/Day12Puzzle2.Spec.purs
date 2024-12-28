module Day12Puzzle2.Spec where

import Prelude

import Data.Array as Array
import Data.String.CodeUnits as String
import Day12Puzzle2 (nSidesOfRegion, regionFor, regions, solve)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

spec :: Spec Unit
spec = do
  describe "Day12Puzzle2" do

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

    it "counts the sides of a region" do
      (nSidesOfRegion <$> regions testInput) `shouldEqual` [4, 4, 8, 4, 4]

    it "solves for the test input" do
      solve testInput `shouldEqual` 80

    let 
      testInputWithInnerRegions = 
        [ "OOOOO"
        , "OXOXO"
        , "OXOXO"
        , "OOOOO"
        , "OXOXO"
        , "OOOOO"
        ]

    it "counts the sides of a region with inner regions" do
      (nSidesOfRegion <$> regions (map String.toCharArray testInputWithInnerRegions)) `shouldEqual` [20, 4, 4, 4, 4]

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
      solve (map String.toCharArray biggerTestInput) `shouldEqual` 1206

