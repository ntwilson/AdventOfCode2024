module Day4Puzzle2 where

import Prelude

import Data.Array as Array
import Data.Foldable (indexl, sum)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Maybe (Maybe(..))
import Data.Set as Set
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


solve :: Array String -> Int
solve input = 
  chars 
  # mapWithIndex (\rowNum row -> row # mapWithIndex (\colNum char -> charIsCross rowNum colNum char)) 
  <#> Array.filter identity
  <#> Array.length 
  # sum

  where 
  chars = input <#> CodeUnits.toCharArray
  charIsCross rowNum colNum 'A' =
    let
      downcross = 
        Set.fromFoldable 
          [ chars # indexl (rowNum - 1) >>= indexl (colNum - 1)
          , chars # indexl (rowNum + 1) >>= indexl (colNum + 1)
          ]

      upcross = 
        Set.fromFoldable 
          [ chars # indexl (rowNum - 1) >>= indexl (colNum + 1)
          , chars # indexl (rowNum + 1) >>= indexl (colNum - 1)
          ]

    in 
      downcross == upcross && downcross == Set.fromFoldable [Just 'M', Just 'S']

  charIsCross _ _ _ = false

run :: Aff Unit
run = do
  inputs <- getInputs
  let
    ans = solve inputs

  log ("Day 4, Puzzle 2: " <> show ans)

