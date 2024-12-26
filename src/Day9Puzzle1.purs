module Day9Puzzle1 where

import Prelude

import Data.Array ((..))
import Data.Array as Array
import Data.Foldable (sum)
import Data.Int as Int
import Data.Maybe (Maybe(..), isJust, isNothing)
import Data.String as String
import Data.String.CodeUnits (singleton)
import Data.String.CodeUnits as String
import Data.Tuple.Nested ((/\))
import Effect.Aff (Aff)
import Effect.Class.Console (log)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile)

getInputs :: Aff String
getInputs = do
  contents <- readTextFile UTF8 "inputs/day9.txt"
  pure $ String.trim contents

parseInput :: String -> Array Int
parseInput contents =
  contents
    # String.toCharArray
    # map (Int.fromString <<< singleton)
    # Array.catMaybes

type MemoryCell = Maybe Int

processMemoryMap :: Array Int -> Array MemoryCell
processMemoryMap xs = do
  { i, x } <- indexed xs
  let cell = if i `mod` 2 == 0 then Just (i / 2) else Nothing
  Array.replicate x $ cell

  where
  indexed ys =
    Array.zipWith { i: _, x: _ } (0 .. (Array.length ys - 1)) ys

rearrange :: Array MemoryCell -> Array MemoryCell
rearrange memMap = 
  let 
    firstNothing = Array.findIndex isNothing memMap
    lastSomething = Array.findLastIndex isJust memMap
  in case firstNothing, lastSomething of
    Just fn, Just ls | fn < ls -> 
        rearrange $ Array.updateAtIndices [fn /\ join (Array.index memMap ls), ls /\ Nothing] memMap
    _, _ -> memMap



solve :: Array MemoryCell -> Number
solve memMap = 
  let
    rearranged = rearrange memMap
  in 
    Array.catMaybes rearranged
    # Array.zipWith (*) (0 .. (Array.length rearranged - 1))
    <#> Int.toNumber
    # sum

run :: Aff Unit
run = do
  contents <- getInputs
  let
    inputs = parseInput contents
    memMap = processMemoryMap inputs
    ans = solve memMap

  log ("Day 9, Puzzle 1: " <> show ans)

