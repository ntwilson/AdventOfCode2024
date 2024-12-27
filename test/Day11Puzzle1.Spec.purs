module Day11Puzzle1.Spec where

import Prelude

import Day11Puzzle1 (getInputs, solve)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

spec :: Spec Unit
spec = do
  describe "Day11Puzzle1" do

    it "can load the test input" do
      inputs <- getInputs
      inputs `shouldEqual` [0.0, 37551.0, 469.0, 63.0, 1.0, 791606.0, 2065.0, 9983586.0]
       
    let
      testInput = [ 125.0, 17.0 ]

    it "can solve for the test input" do
      solve testInput `shouldEqual` 55312.0

