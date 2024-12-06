module Day3Puzzle1 where

import Prelude

import Control.Alt ((<|>))
import Control.Monad.Error.Class (class MonadThrow, liftEither)
import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.Foldable (sum)
import Data.Maybe (Maybe(..))
import Data.Tuple (uncurry)
import Data.Tuple.Nested ((/\), type (/\))
import Effect.Aff (Aff, error)
import Effect.Class.Console (log)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile)
import Parsing (parseErrorMessage, runParser)
import Parsing.Combinators (try)
import Parsing.Combinators.Array (many)
import Parsing.String as Parser
import Parsing.String.Basic as Parser

getInputs :: Aff String
getInputs = do
  readTextFile UTF8 "inputs/day3.txt"

parseInput :: ∀ m. MonadThrow String m => String -> m _
parseInput s = liftEither $ lmap parseErrorMessage $ runParser s $ Array.catMaybes <$> do
  many ((Just <$> try parseCommand) <|> (Nothing <$ Parser.anyChar))

  where
  parseCommand = do
    _ <- Parser.string "mul("
    l <- Parser.intDecimal
    _ <- Parser.string ","
    r <- Parser.intDecimal
    _ <- Parser.string ")"
    pure (l /\ r)

solve :: Array (Int /\ Int) -> _
solve inputs =
  inputs <#> uncurry (*) # sum

run :: Aff Unit
run = do
  contents <- getInputs
  inputs <- parseInput contents # lmap error # liftEither
  let
    ans = solve inputs

  log ("Day 3, Puzzle 1: " <> show ans)

