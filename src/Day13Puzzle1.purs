module Day13Puzzle1 where

import Prelude

import Control.Alternative as Alternative
import Data.Array ((..))
import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.Foldable (minimumBy, sum)
import Data.Maybe (Maybe)
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

type Button = { x :: Int, y :: Int }
type Machine = { buttonA :: Button, buttonB :: Button, prize :: { x :: Int, y :: Int } }

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
    x <- Parser.intDecimal
    _ <- Parsing.string ", Y+"
    y <- Parser.intDecimal
    Parser.skipSpaces
    pure { x, y }

  parsePrize = do
    _ <- Parsing.string "Prize: X="
    x <- Parser.intDecimal
    _ <- Parsing.string ", Y="
    y <- Parser.intDecimal
    Parser.skipSpaces
    pure { x, y }

type Solution = { buttonAPresses :: Int, buttonBPresses :: Int }

scoreSolution :: Solution -> Int
scoreSolution { buttonAPresses, buttonBPresses } = 3*buttonAPresses + buttonBPresses

solveMachine :: Machine -> Maybe Solution
solveMachine { buttonA, buttonB, prize } =
  solutions # minimumBy (comparing scoreSolution)
  where

  solutions = 
    let
      aPresses = 0 .. min (prize.x / buttonA.x) (prize.y / buttonA.y)
      bPresses = 0 .. min (prize.x / buttonB.x) (prize.y / buttonB.y)

    in do
      a <- aPresses
      b <- bPresses
      Alternative.guard (a*buttonA.x + b*buttonB.x == prize.x && a*buttonA.y + b*buttonB.y == prize.y)
      pure { buttonAPresses: a, buttonBPresses: b }
      

solve :: Array Machine -> Int
solve input = input <#> solveMachine # Array.catMaybes <#> scoreSolution # sum

run :: Aff Unit
run = do
  contents <- getInputs
  input <- parseInputs contents
  let
    ans = solve input

  log ("Day 13, Puzzle 1: " <> show ans)

