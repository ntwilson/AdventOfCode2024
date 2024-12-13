module Day5Puzzle2.Spec where

import Prelude

import Data.Array as Array
import Day5Puzzle2 (OrderingRule(..), fixUpdate, isUpdateValid, solve)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

spec :: Spec Unit
spec = do
  describe "Day5Puzzle1" do
    let

      parsedRules =
        [ OrderingRule 47 53
        , OrderingRule 97 13
        , OrderingRule 97 61
        , OrderingRule 97 47
        , OrderingRule 75 29
        , OrderingRule 61 13
        , OrderingRule 75 53
        , OrderingRule 29 13
        , OrderingRule 97 29
        , OrderingRule 53 29
        , OrderingRule 61 53
        , OrderingRule 97 53
        , OrderingRule 61 29
        , OrderingRule 47 13
        , OrderingRule 75 47
        , OrderingRule 97 75
        , OrderingRule 47 61
        , OrderingRule 75 61
        , OrderingRule 47 29
        , OrderingRule 75 13
        , OrderingRule 53 13
        ]

      parsedUpdates =
        [ [ 75, 47, 61, 53, 29 ]
        , [ 97, 61, 53, 29, 13 ]
        , [ 75, 29, 13 ]
        , [ 75, 97, 47, 61, 53 ]
        , [ 61, 13, 29 ]
        , [ 97, 13, 75, 29, 47 ]
        ]

    it "can fix a bad update" do
      (Array.filter (not isUpdateValid parsedRules) parsedUpdates <#> fixUpdate parsedRules) `shouldEqual`
        [ [ 97, 75, 47, 61, 53 ]
        , [ 61, 29, 13 ]
        , [ 97, 75, 47, 29, 13 ]
        ]

    it "can solve the test input" do
      solve { rules: parsedRules, updates: parsedUpdates } `shouldEqual` 123

