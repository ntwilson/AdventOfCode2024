module Day3Puzzle2.Spec where

import Prelude

import Data.Array.NonEmpty.Internal (NonEmptyArray(..))
import Data.Either (Either(..))
import Data.Tuple.Nested ((/\))
import Day3Puzzle2 (InputToken(..), enabledInputs, groupInputs, parseInput)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

spec :: Spec Unit
spec = do
  describe "Day3Puzzle2" do
    let
      testInput =
        "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))"

    it "should parse the input given in the puzzle description" do
      parseInput testInput `shouldEqual` (Right $ [ Mul 2 4, Disable, Mul 5 5, Mul 11 8, Enable, Mul 8 5 ])

    it "should solve the input by excluding the disabled middle commands" do
      (groupInputs <$> parseInput testInput) `shouldEqual` (Right $ NonEmptyArray <$> [ [ Mul 2 4 ], [ Disable, Mul 5 5, Mul 11 8 ], [ Enable, Mul 8 5 ] ])
      (enabledInputs <$> parseInput testInput) `shouldEqual` (Right [ (2 /\ 4), (8 /\ 5) ])

