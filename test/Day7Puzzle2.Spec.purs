module Day7Puzzle2.Spec where

import Prelude

import Day7Puzzle2 (solve, (|||))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

spec :: Spec Unit
spec = do
  describe "Day7Puzzle2" do
    let
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

    it "should be able to solve the puzzle" do
      solve parsedEquations `shouldEqual` 11387.0

    it "can concatenate two numbers by smashing their digits together" do
      (12.0 ||| 345.0 ||| 67.0) `shouldEqual` 1234567.0

