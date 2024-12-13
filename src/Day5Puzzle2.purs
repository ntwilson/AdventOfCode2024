module Day5Puzzle2 where

import Prelude

import Control.Alt ((<|>))
import Control.Monad.Error.Class (class MonadThrow, liftEither)
import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.Either (note)
import Data.Foldable (sum)
import Data.Generic.Rep (class Generic)
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Data.String (Pattern(..))
import Data.String as String
import Data.Traversable (traverse)
import Effect.Aff (Aff, error)
import Effect.Class.Console (log)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile)
import Parsing (parseErrorMessage, runParser)
import Parsing.Combinators (try)
import Parsing.Combinators.Array (many)
import Parsing.String as Parser
import Parsing.String.Basic as Parser

getInputs :: Aff (Array String)
getInputs = do
  contents <- readTextFile UTF8 "inputs/day5.txt"
  pure $ String.split (Pattern "\n") contents

data OrderingRule = OrderingRule Int Int

derive instance Eq OrderingRule
derive instance Generic OrderingRule _
instance Show OrderingRule where
  show = genericShow

type Update = Array Int

type PuzzleInput =
  { rules :: Array OrderingRule
  , updates :: Array Update
  }

parseInput :: ∀ m. MonadThrow String m => Array String -> m PuzzleInput
parseInput input = do
  splitIndex <-
    Array.findIndex (String.null <<< String.trim) input
      # note "No blank line found in input to separate stacks and instructions"
      # liftEither

  let { before, after } = Array.splitAt splitIndex input
  rules <- traverse parseRule (before # Array.filter (not <<< String.null))
  updates <- traverse parseUpdate (after # Array.filter (not <<< String.null))

  pure { rules, updates }

  where
  parseRule s = liftEither $ lmap parseErrorMessage $ runParser s do
    preceding <- Parser.intDecimal
    _ <- Parser.string "|"
    succeeding <- Parser.intDecimal
    pure $ OrderingRule preceding succeeding

  parseUpdate s = liftEither $ lmap parseErrorMessage $ runParser s do
    many (try (Parser.intDecimal <* (void (Parser.string ",") <|> Parser.eof <|> void Parser.whiteSpace)))

isUpdateValid :: Array OrderingRule -> Update -> Boolean
isUpdateValid rules update =
  Array.all ruleIsUpheld rules

  where
  ruleIsUpheld (OrderingRule preceding succeeding) =
    case Array.elemIndex preceding update, Array.elemIndex succeeding update of
      Just p, Just s -> p < s
      _, _ -> true
    
middleChar :: Update -> Maybe Int
middleChar update =
  Array.index update (Array.length update `div` 2)

fixUpdate :: Array OrderingRule -> Update -> Update
fixUpdate rules update = go Nil rules update
  where
  go acc rules update =
    case onlyPrecedingRule of 
      Nothing -> Array.reverse (Array.fromFoldable acc) <> update
      Just (OrderingRule preceding _) -> 
        go (preceding : acc) rulesThatMatter (Array.delete preceding update)

    where
    rulesThatMatter = 
      rules
      # Array.filter (\(OrderingRule preceding succeeding) -> Array.elem preceding update && Array.elem succeeding update)

    onlyPrecedingRule = 
      rulesThatMatter 
      # Array.find (\(OrderingRule preceding _) -> 
        rulesThatMatter # not Array.any (\(OrderingRule _ succeeding) -> preceding == succeeding))

solve :: PuzzleInput -> Int
solve { rules, updates } =
  updates
    # Array.filter (not isUpdateValid rules)
    <#> fixUpdate rules
    # Array.mapMaybe middleChar
    # sum

run :: Aff Unit
run = do
  contents <- getInputs
  inputs <- parseInput contents # lmap error # liftEither
  let
    ans = solve inputs

  log ("Day 5, Puzzle 1: " <> show ans)

