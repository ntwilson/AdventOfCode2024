module Day7Puzzle2 where

import Prelude

import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.Foldable (sum)
import Data.Maybe (fromJust)
import Data.Number as Number
import Data.Number.Format as Number
import Data.String (Pattern(..))
import Data.String as String
import Data.Traversable (traverse)
import Effect.Aff (Aff)
import Effect.Class.Console (log)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile)
import Parsing (parseErrorMessage, runParser)
import Parsing.Combinators.Array (many)
import Parsing.String as Parser
import Parsing.String.Basic as Parser
import Partial.Unsafe (unsafePartial)
import Throws (class Throws, liftEither)

getInputs :: Aff (Array String)
getInputs = do
  contents <- readTextFile UTF8 "inputs/day7.txt"
  let 
    lines = String.split (Pattern "\n") contents
  pure (Array.filter (not String.null) lines)

type Equation = { left :: Number, right :: Array Number }

parseInput :: ∀ m. Monad m => Throws String m => Array String -> m (Array Equation)
parseInput input = traverse parseEquation input

  where
  parseEquation s = liftEither $ lmap parseErrorMessage $ runParser s do
    left <- Parser.number
    _ <- Parser.string ":"
    right <- many (Parser.skipSpaces *> Parser.number)
    pure { left, right }

concatenate :: Number -> Number -> Number
concatenate a b = unsafePartial $ fromJust $ Number.fromString (justIntPart a <> justIntPart b)
  where
  justIntPart = Number.toStringWith $ Number.fixed 0

infixl 3 concatenate as ||| 

possibleCombinations :: Array Number -> Array Number
possibleCombinations operands = 
  operands 
  # Array.foldl (\acc x -> case acc of
      [] -> [x]
      _ -> map (x + _) acc <> map (x * _) acc <> map (_ ||| x) acc) []

isValid :: Equation -> Boolean
isValid {left, right} = Array.elem left $ possibleCombinations right
  
solve :: Array Equation -> Number
solve input = Array.filter isValid input <#> _.left # sum

run :: Aff Unit
run = do
  contents <- getInputs
  inputs <- parseInput contents
  let
    ans = solve inputs

  log ("Day 7, Puzzle 2: " <> show ans)
