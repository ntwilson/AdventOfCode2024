module Day5Puzzle1.Spec where

import Prelude

import Data.Array as Array
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Day5Puzzle1 (OrderingRule(..), isUpdateValid, middleChar, parseInput)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

spec :: Spec Unit
spec = do
  describe "Day5Puzzle1" do
    let
      testInput =
        [ "47|53"
        , "97|13"
        , "97|61"
        , "97|47"
        , "75|29"
        , "61|13"
        , "75|53"
        , "29|13"
        , "97|29"
        , "53|29"
        , "61|53"
        , "97|53"
        , "61|29"
        , "47|13"
        , "75|47"
        , "97|75"
        , "47|61"
        , "75|61"
        , "47|29"
        , "75|13"
        , "53|13"
        , ""
        , "75,47,61,53,29"
        , "97,61,53,29,13"
        , "75,29,13"
        , "75,97,47,61,53"
        , "61,13,29"
        , "97,13,75,29,47"
        ]

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

    it "should be able to parse the input" do
      parseInput testInput `shouldEqual`
        ( Right
            { rules: parsedRules
            , updates: parsedUpdates
            }
        )

    it "can tell if an update is valid or not" do
      Array.filter (isUpdateValid parsedRules) parsedUpdates `shouldEqual` 
        [ [ 75,47,61,53,29 ]
        , [ 97,61,53,29,13 ]
        , [ 75,29,13 ]
        ]

    it "can find the middle element of each update" do
      (middleChar <$> parsedUpdates) `shouldEqual`
        [ Just 61
        , Just 53
        , Just 29
        , Just 47
        , Just 13
        , Just 75
        ]
