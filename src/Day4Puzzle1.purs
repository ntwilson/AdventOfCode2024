module Day4Puzzle1 where

import Prelude

import Data.Array ((..))
import Data.Array as Array
import Data.Foldable (sum)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (Pattern(..))
import Data.String as String
import Data.String.CodeUnits as CodeUnits
import Effect.Aff (Aff)
import Effect.Class.Console (log)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile)

getInputs :: Aff (Array String)
getInputs = do
  contents <- readTextFile UTF8 "inputs/day4.txt"

  let
    lines = String.split (Pattern "\n") contents
    nonEmptyLines = Array.filter (not <<< String.null) lines

  pure nonEmptyLines

backwards :: Array String -> Array String
backwards = map (CodeUnits.fromCharArray <<< Array.reverse <<< CodeUnits.toCharArray)

diagonalDownForwards :: Array String -> Array String
diagonalDownForwards inputs = 
  case Array.head inputs of 
    Nothing -> []
    Just firstLine -> 
      let
        lineLength = String.length firstLine
      in
        (0 .. lineLength <#> line) <> (negate lineLength .. -1 <#> line)
        # Array.filter (String.length >>> (_ >= 4))

  where
  line :: Int -> String
  line index =
    inputs
    # mapWithIndex (\i row ->
      CodeUnits.charAt (i + index) row)
    # Array.catMaybes
    # CodeUnits.fromCharArray

diagonalUpBackwards :: Array String -> Array String
diagonalUpBackwards = diagonalDownForwards >>> backwards

diagonalDownBackwards :: Array String -> Array String
diagonalDownBackwards = backwards >>> diagonalDownForwards 

diagonalUpForwards :: Array String -> Array String
diagonalUpForwards = backwards >>> diagonalDownForwards >>> backwards

columns :: Array String -> Array String
columns inputs = 
  let
    lineLength = inputs
      # Array.head
      <#> String.length
      # fromMaybe 0
  in
    (0 .. lineLength <#> column)

  where
  column :: Int -> String
  column index =
    inputs
    # map (\row -> CodeUnits.charAt index row)
    # Array.catMaybes
    # CodeUnits.fromCharArray

columnsBackwards :: Array String -> Array String
columnsBackwards = columns >>> backwards

xmasInstances :: String -> Int
xmasInstances input = 
  case String.indexOf (Pattern "XMAS") input of
    Nothing -> 0
    Just index -> 1 + xmasInstances (String.drop (index + 4) input)

solve :: Array String -> Int
solve input =
  sum (xmasInstances <$> input) 
  + sum (xmasInstances <$> (backwards input))
  + sum (xmasInstances <$> (columns input)) 
  + sum (xmasInstances <$> (columnsBackwards input)) 
  + sum (xmasInstances <$> (diagonalDownForwards input)) 
  + sum (xmasInstances <$> (diagonalDownBackwards input)) 
  + sum (xmasInstances <$> (diagonalUpForwards input)) 
  + sum (xmasInstances <$> (diagonalUpBackwards input))

run :: Aff Unit
run = do
  inputs <- getInputs
  let
    ans = solve inputs

  log ("Day 4, Puzzle 1: " <> show ans)

