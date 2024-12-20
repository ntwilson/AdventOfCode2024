module Day7Puzzle1.Spec where

import Prelude

import Data.Either (Either(..))
import Day7Puzzle1 (parseInput, solve)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

spec :: Spec Unit
spec = do
  describe "Day7Puzzle1" do
    let
      testInput =
        [ "190: 10 19"
        , "3267: 81 40 27"
        , "83: 17 5"
        , "156: 15 6"
        , "7290: 6 8 6 15"
        , "161011: 16 10 13"
        , "192: 17 8 14"
        , "21037: 9 7 18 13"
        , "292: 11 6 16 20"
        ]

      parsedEquations =
        [ { left: 190.0, right: [ 10.0, 19.0 ] }
        , { left: 3267.0, right: [ 81.0, 40.0, 27.0 ] }
        , { left: 83.0, right: [ 17.0, 5.0 ] }
        , { left: 156.0, right: [ 15.0, 6.0 ] }
        , { left: 7290.0, right: [ 6.0, 8.0, 6.0, 15.0 ] }
        , { left: 161011.0, right: [ 16.0, 10.0, 13.0 ] }
        , { left: 192.0, right: [ 17.0, 8.0, 14.0 ] }
        , { left: 21037.0, right: [ 9.0, 7.0, 18.0, 13.0 ] }
        , { left: 292.0, right: [ 11.0, 6.0, 16.0, 20.0 ] }
        ]

    it "should be able to parse the input" do
      (parseInput testInput :: Either String _) `shouldEqual` Right parsedEquations

    it "should be able to solve the puzzle" do
      solve parsedEquations `shouldEqual` 3749.0

