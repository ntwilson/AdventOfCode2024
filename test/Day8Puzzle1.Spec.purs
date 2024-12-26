module Day8Puzzle1.Spec where

import Prelude

import Data.Map as Map
import Data.Set as Set
import Data.Tuple.Nested ((/\))
import Day8Puzzle1 (MapCoordinate(..), pairs, parseInput, processMap, solve)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

spec :: Spec Unit
spec = do
  describe "Day8Puzzle1" do

    let
      testInput =
        [ "............"
        , "........0..."
        , ".....0......"
        , ".......0...."
        , "....0......."
        , "......A....."
        , "............"
        , "............"
        , "........A..."
        , ".........A.."
        , "............"
        , "............"
        ]

      parsedInput =
        [ [ Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty ]
        , [ Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Antenna '0', Empty, Empty, Empty ]
        , [ Empty, Empty, Empty, Empty, Empty, Antenna '0', Empty, Empty, Empty, Empty, Empty, Empty ]
        , [ Empty, Empty, Empty, Empty, Empty, Empty, Empty, Antenna '0', Empty, Empty, Empty, Empty ]
        , [ Empty, Empty, Empty, Empty, Antenna '0', Empty, Empty, Empty, Empty, Empty, Empty, Empty ]
        , [ Empty, Empty, Empty, Empty, Empty, Empty, Antenna 'A', Empty, Empty, Empty, Empty, Empty ]
        , [ Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty ]
        , [ Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty ]
        , [ Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Antenna 'A', Empty, Empty, Empty ]
        , [ Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Antenna 'A', Empty, Empty ]
        , [ Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty ]
        , [ Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty ]
        ]
    it "can parse the test input" do
      parseInput testInput `shouldEqual` parsedInput

    it "can process the test input" do
      processMap parsedInput `shouldEqual`
        { width: 12
        , height: 12
        , antennas: Map.fromFoldable
            [ '0' /\ Set.fromFoldable [ { row: 1, col: 8 }, { row: 2, col: 5 }, { row: 3, col: 7 }, { row: 4, col: 4 } ]
            , 'A' /\ Set.fromFoldable [ { row: 5, col: 6 }, { row: 8, col: 8 }, { row: 9, col: 9 } ]
            ]
        }

    it "can pair up elements of a set" do
      let
        testSet = Set.fromFoldable [ 1, 2, 3, 4 ]

      pairs testSet `shouldEqual`
        Set.fromFoldable
          [ { lesser: 1, greater: 2 }
          , { lesser: 1, greater: 3 }
          , { lesser: 1, greater: 4 }
          , { lesser: 2, greater: 3 }
          , { lesser: 2, greater: 4 }
          , { lesser: 3, greater: 4 }
          ]

    it "can solve for the test input" do
      (parseInput testInput # processMap # solve) `shouldEqual` 14
