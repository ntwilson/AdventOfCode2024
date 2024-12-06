module Day3Puzzle2 where

import Prelude

import Control.Alt ((<|>))
import Control.Monad.Error.Class (class MonadThrow, liftEither)
import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NonEmptyArray
import Data.Bifunctor (lmap)
import Data.Foldable (sum)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
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

data InputToken
  = Mul Int Int
  | Disable
  | Enable

derive instance Eq InputToken
derive instance Generic InputToken _
instance Show InputToken where
  show = genericShow

parseInput :: ∀ m. MonadThrow String m => String -> m _
parseInput s = liftEither $ lmap parseErrorMessage $ runParser s $ Array.catMaybes <$> do
  many ((Just <$> (try parseCommand <|> try parseDisable <|> try parseEnable)) <|> (Nothing <$ Parser.anyChar))

  where
  parseCommand = do
    _ <- Parser.string "mul("
    l <- Parser.intDecimal
    _ <- Parser.string ","
    r <- Parser.intDecimal
    _ <- Parser.string ")"
    pure (Mul l r)

  parseDisable = Parser.string "don't()" $> Disable
  parseEnable = Parser.string "do()" $> Enable

groupInputs :: Array InputToken -> Array (NonEmptyArray InputToken)
groupInputs inputs = 
  inputs
    # Array.groupBy
        ( \_ finisher -> case finisher of
            Enable -> false
            Disable -> false
            Mul _ _ -> true
        )

enabledInputs :: Array InputToken -> Array (Int /\ Int)
enabledInputs inputs =
  enabledGroups
    # map
        ( NonEmptyArray.mapMaybe
            ( case _ of
                Mul l r -> Just (l /\ r)
                _ -> Nothing
            )
        )
    # Array.concat

  where
  enabledGroups :: Array (NonEmptyArray InputToken)
  enabledGroups = 
    groupInputs inputs
      # Array.filter
          ( \a -> case NonEmptyArray.head a of
              Enable -> true
              Mul _ _ -> true
              Disable -> false
          )

solve :: Array InputToken -> _
solve inputs =
  sum enabledProducts
  where

  enabledProducts =
    enabledInputs inputs <#> uncurry (*)

run :: Aff Unit
run = do
  contents <- getInputs
  inputs <- parseInput contents # lmap error # liftEither
  let
    ans = solve inputs

  log ("Day 3, Puzzle 2: " <> show ans)

