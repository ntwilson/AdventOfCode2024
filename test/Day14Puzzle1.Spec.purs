module Day14Puzzle1.Spec where

import Prelude

import Data.Either (Either(..), isRight)
import Day14Puzzle1 (getInputs, moveRobot, parseInputs, partitionQuadrants, solve)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual, shouldSatisfy)

spec :: Spec Unit
spec = do
  describe "Day14Puzzle1" do

    let
      testInput =
        [ "p=0,4 v=3,-3"
        , "p=6,3 v=-1,-3"
        , "p=10,3 v=-1,2"
        , "p=2,0 v=2,-1"
        , "p=0,0 v=1,3"
        , "p=3,0 v=-2,-2"
        , "p=7,6 v=-1,-3"
        , "p=3,0 v=-1,-2"
        , "p=9,3 v=2,3"
        , "p=7,3 v=-1,2"
        , "p=2,4 v=2,-3"
        , "p=9,5 v=-3,-3"
        ]

    it "parses the test input" do
      (parseInputs testInput :: Either String _) `shouldEqual` Right
        [ { position: { x: 0, y: 4 }, velocity: { x: 3, y: -3 } }
        , { position: { x: 6, y: 3 }, velocity: { x: -1, y: -3 } }
        , { position: { x: 10, y: 3 }, velocity: { x: -1, y: 2 } }
        , { position: { x: 2, y: 0 }, velocity: { x: 2, y: -1 } }
        , { position: { x: 0, y: 0 }, velocity: { x: 1, y: 3 } }
        , { position: { x: 3, y: 0 }, velocity: { x: -2, y: -2 } }
        , { position: { x: 7, y: 6 }, velocity: { x: -1, y: -3 } }
        , { position: { x: 3, y: 0 }, velocity: { x: -1, y: -2 } }
        , { position: { x: 9, y: 3 }, velocity: { x: 2, y: 3 } }
        , { position: { x: 7, y: 3 }, velocity: { x: -1, y: 2 } }
        , { position: { x: 2, y: 4 }, velocity: { x: 2, y: -3 } }
        , { position: { x: 9, y: 5 }, velocity: { x: -3, y: -3 } }
        ]

    it "parses the real input" do
      inputs <- getInputs
      (parseInputs inputs :: Either String _) `shouldSatisfy` isRight

    it "advances a robot for a single turn" do
      let
        startingRobot = { position: { x: 2, y: 4 }, velocity: { x: 2, y: -3 } }
        move = moveRobot { width: 11, height: 7 }

      move startingRobot `shouldEqual` startingRobot { position = { x: 4, y: 1 } }
      move (move startingRobot) `shouldEqual` startingRobot { position = { x: 6, y: 5 } }

    it "splits the robots into quadrants" do
      let
        -- ......2..1.
        -- ...........
        -- 1..........
        -- .11........
        -- .....1.....
        -- ...12......
        -- .1....1....
        velocity = { x: 0, y: 0 }
        robots =
          [ { position: { x: 6, y: 0 }, velocity }
          , { position: { x: 6, y: 0 }, velocity }
          , { position: { x: 9, y: 0 }, velocity }
          , { position: { x: 0, y: 2 }, velocity }
          , { position: { x: 1, y: 3 }, velocity }
          , { position: { x: 2, y: 3 }, velocity }
          , { position: { x: 5, y: 4 }, velocity }
          , { position: { x: 3, y: 5 }, velocity }
          , { position: { x: 4, y: 5 }, velocity }
          , { position: { x: 4, y: 5 }, velocity }
          , { position: { x: 1, y: 6 }, velocity }
          , { position: { x: 6, y: 6 }, velocity }
          ]
        bounds = { width: 11, height: 7 }
        { q1, q2, q3, q4 } = partitionQuadrants bounds robots

      -- ..... 2..1.
      -- ..... .....
      -- 1.... .....
      --            
      -- ..... .....
      -- ...12 .....
      -- .1... 1....

      q1 `shouldEqual` [ { position: { x: 0, y: 2 }, velocity } ]
      q2 `shouldEqual`
        [ { position: { x: 6, y: 0 }, velocity }
        , { position: { x: 6, y: 0 }, velocity }
        , { position: { x: 9, y: 0 }, velocity }
        ]

      q3 `shouldEqual`
        [ { position: { x: 3, y: 5 }, velocity }
        , { position: { x: 4, y: 5 }, velocity }
        , { position: { x: 4, y: 5 }, velocity }
        , { position: { x: 1, y: 6 }, velocity }
        ]

      q4 `shouldEqual` [ { position: { x: 6, y: 6 }, velocity } ]

    it "solves for the test input" do
      (solve { width: 11, height: 7 } <$> parseInputs testInput :: Either String _) `shouldEqual` Right 12

