module Day13Puzzle1.Spec where

import Prelude

import Data.Either (Either(..), isRight)
import Data.Maybe (Maybe(..))
import Day13Puzzle1 (getInputs, parseInputs, solve, solveMachine)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual, shouldSatisfy)

spec :: Spec Unit
spec = do
  describe "Day13Puzzle1" do

    let
      testInput =
        "Button A: X+94, Y+34\
        \Button B: X+22, Y+67\
        \Prize: X=8400, Y=5400\
        \ \
        \Button A: X+26, Y+66\
        \Button B: X+67, Y+21\
        \Prize: X=12748, Y=12176\
        \ \
        \Button A: X+17, Y+86\
        \Button B: X+84, Y+37\
        \Prize: X=7870, Y=6450\
        \ \
        \Button A: X+69, Y+23\
        \Button B: X+27, Y+71\
        \Prize: X=18641, Y=10279"

    it "parses the test input" do
      (parseInputs testInput :: Either String _) `shouldEqual` Right 
        [ { buttonA: { x: 94, y: 34 }
          , buttonB: { x: 22, y: 67 }
          , prize: { x: 8400, y: 5400 }
          }
        , { buttonA: { x: 26, y: 66 }
          , buttonB: { x: 67, y: 21 }
          , prize: { x: 12748, y: 12176 }
          }
        , { buttonA: { x: 17, y: 86 }
          , buttonB: { x: 84, y: 37 }
          , prize: { x: 7870, y: 6450 }
          }
        , { buttonA: { x: 69, y: 23 }
          , buttonB: { x: 27, y: 71 }
          , prize: { x: 18641, y: 10279 }
          }
        ]

    it "parses the real input" do
      inputs <- getInputs
      (parseInputs inputs :: Either String _) `shouldSatisfy` isRight

    it "solves a single machine" do
       solveMachine { buttonA: { x: 94, y: 34 }, buttonB: { x: 22, y: 67 }, prize: { x: 8400, y: 5400 } } `shouldEqual` 
         Just { buttonAPresses: 80, buttonBPresses: 40 }


    it "solves for the test input" do
      (solve <$> parseInputs testInput :: Either String _) `shouldEqual` Right 480

