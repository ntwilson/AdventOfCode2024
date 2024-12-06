module Day3Puzzle1.Spec where

import Prelude

import Data.Either (Either(..))
import Data.Tuple.Nested ((/\))
import Day3Puzzle1 (parseInput)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

spec :: Spec Unit
spec = do
  describe "Day3Puzzle1" do
   it "should parse the input given in the puzzle description" do
     let
       testInput = 
         "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))"
     parseInput testInput `shouldEqual` (Right $ [(2/\4), (5/\5), (11/\8), (8/\5)])
