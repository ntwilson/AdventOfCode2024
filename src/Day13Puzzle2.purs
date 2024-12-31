module Day13Puzzle2 where

import Prelude

import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.Foldable (minimumBy, sum)
import Data.List.Lazy as List
import Data.Maybe (Maybe)
import Data.Number as Number
import Effect.Aff (Aff)
import Effect.Class.Console (log)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile)
import Parsing (parseErrorMessage, runParser)
import Parsing.Combinators.Array (many)
import Parsing.String as Parsing
import Parsing.String.Basic as Parser
import Throws (class Throws, liftEither)

getInputs :: Aff String
getInputs = do
  contents <- readTextFile UTF8 "inputs/day13.txt"
  pure contents

type Button = { x :: Number, y :: Number }
type Machine = { buttonA :: Button, buttonB :: Button, prize :: { x :: Number, y :: Number } }

parseInputs :: ∀ m. Applicative m => Throws String m => String -> m (Array Machine)
parseInputs s = liftEither $ lmap parseErrorMessage $ runParser s $ many parseMachine
  where
  parseMachine = do
    buttonA <- parseButton "A"
    buttonB <- parseButton "B"
    prize <- parsePrize
    pure { buttonA, buttonB, prize }

  parseButton id = do
    _ <- Parsing.string ("Button " <> id <> ": X+")
    x <- Parser.number
    _ <- Parsing.string ", Y+"
    y <- Parser.number
    Parser.skipSpaces
    pure { x, y }

  parsePrize = do
    _ <- Parsing.string "Prize: X="
    x <- Parser.number
    _ <- Parsing.string ", Y="
    y <- Parser.number
    Parser.skipSpaces
    pure { x: x + 10000000000000.0, y: y + 10000000000000.0 }

type Solution = { buttonAPresses :: Number, buttonBPresses :: Number }

scoreSolution :: Solution -> Number
scoreSolution { buttonAPresses, buttonBPresses } = 3.0 * buttonAPresses + buttonBPresses

almostEqual :: Number -> Number -> Boolean
almostEqual a b = Number.abs (a - b) < 0.0000000001

infix 4 almostEqual as ~=

solveMachine :: Machine -> Maybe Solution
solveMachine { buttonA, buttonB, prize } =
  solutions # minimumBy (comparing scoreSolution)
  where

  -- a * ax + b * bx = x
  -- a * ay + b * by = y

  -- a = x / ax - b * bx / ax
  -- a = y / ay - b * by / ay

  sys1YIntercept = prize.x / buttonA.x
  sys2YIntercept = prize.y / buttonA.y
  sys1Slope = -buttonB.x / buttonA.x
  sys2Slope = -buttonB.y / buttonA.y

  solutions | sys1Slope ~= sys2Slope && sys1YIntercept ~= sys2YIntercept =
    List.iterate (_ + 1.0) 0.0
      # map
          ( \buttonBPresses ->
              let
                buttonAPresses = (prize.x - buttonBPresses * buttonB.x) / buttonA.x
              in
                { buttonAPresses, buttonBPresses }
          )
      # List.filter (\{ buttonAPresses } -> buttonAPresses == Number.round buttonAPresses)
      # List.takeWhile (\{ buttonAPresses } -> buttonAPresses >= 0.0)
      # Array.fromFoldable
  solutions | sys1Slope ~= sys2Slope = []
  solutions | otherwise =
    -- a = (x - b * bx) / ax
    -- ((x - b * bx) / ax) * ay + b * by = y
    -- (x - b * bx) * ay + b * ax * by = ax * y
    -- x * ay - b * bx * ay + b * ax * by = ax * y
    -- b * (ax * by - bx * ay) = ax * y - x * ay
    -- b = (ax * y - x * ay) / (ax * by - bx * ay)
    let
      buttonBPresses = (buttonA.x * prize.y - prize.x * buttonA.y) / (buttonA.x * buttonB.y - buttonB.x * buttonA.y)
      buttonAPresses = (prize.x - buttonBPresses * buttonB.x) / buttonA.x
    in
      if buttonAPresses == Number.round buttonAPresses && buttonBPresses == Number.round buttonBPresses then
        [ { buttonAPresses, buttonBPresses } ]
      else
        []

solve :: Array Machine -> Number
solve input = input <#> solveMachine # Array.catMaybes <#> scoreSolution # sum

run :: Aff Unit
run = do
  contents <- getInputs
  input <- parseInputs contents
  let
    ans = solve input

  log ("Day 13, Puzzle 2: " <> show ans)

