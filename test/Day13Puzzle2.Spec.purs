module Day13Puzzle2.Spec where

import Prelude

import Data.Either (Either(..), isRight)
import Data.Maybe (Maybe(..))
import Day13Puzzle2 (getInputs, parseInputs, solve, solveMachine)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual, shouldSatisfy)

spec :: Spec Unit
spec = do
  describe "Day13Puzzle2" do

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
        [ { buttonA: { x: 94.0, y: 34.0 }
          , buttonB: { x: 22.0, y: 67.0 }
          , prize: { x: 10000000008400.0, y: 10000000005400.0 }
          }
        , { buttonA: { x: 26.0, y: 66.0 }
          , buttonB: { x: 67.0, y: 21.0 }
          , prize: { x: 10000000012748.0, y: 10000000012176.0 }
          }
        , { buttonA: { x: 17.0, y: 86.0 }
          , buttonB: { x: 84.0, y: 37.0 }
          , prize: { x: 10000000007870.0, y: 10000000006450.0 }
          }
        , { buttonA: { x: 69.0, y: 23.0 }
          , buttonB: { x: 27.0, y: 71.0 }
          , prize: { x: 10000000018641.0, y: 10000000010279.0 }
          }
        ]

    it "parses the real input" do
      inputs <- getInputs
      (parseInputs inputs :: Either String _) `shouldSatisfy` isRight

    it "solves a single machine" do
      solveMachine { buttonA: { x: 94.0, y: 34.0 }, buttonB: { x: 22.0, y: 67.0 }, prize: { x: 8400.0, y: 5400.0 } } `shouldEqual`
        Just { buttonAPresses: 80.0, buttonBPresses: 40.0 }

    it "rejects machines with no solution" do
      solveMachine { buttonA: { x: 26.0, y: 66.0 }, buttonB: { x: 67.0, y: 21.0 }, prize: { x: 12748.0, y: 12176.0 } } `shouldEqual` Nothing

    it "solves for the test input" do
      let
        puzzle1TestInput =
          [ { buttonA: { x: 94.0, y: 34.0 }
            , buttonB: { x: 22.0, y: 67.0 }
            , prize: { x: 8400.0, y: 5400.0 }
            }
          , { buttonA: { x: 26.0, y: 66.0 }
            , buttonB: { x: 67.0, y: 21.0 }
            , prize: { x: 12748.0, y: 12176.0 }
            }
          , { buttonA: { x: 17.0, y: 86.0 }
            , buttonB: { x: 84.0, y: 37.0 }
            , prize: { x: 7870.0, y: 6450.0 }
            }
          , { buttonA: { x: 69.0, y: 23.0 }
            , buttonB: { x: 27.0, y: 71.0 }
            , prize: { x: 18641.0, y: 10279.0 }
            }
          ]

      solve puzzle1TestInput `shouldEqual` 480.0

