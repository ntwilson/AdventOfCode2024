module Day9Puzzle2 where

import Prelude

import Data.Array ((..))
import Data.Array as Array
import Data.Array.NonEmpty as Array.NonEmpty
import Data.Foldable (sum)
import Data.Int as Int
import Data.List (List(..), (:))
import Data.List as List
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
rearrange memMap = go groups memMap
  where
  groups = 
    indexed memMap 
    # Array.reverse 
    # Array.groupBy (\{ x: a } { x: b } -> a == b)
    # Array.filter (Array.NonEmpty.head >>> _.x >>> isJust)
    # List.fromFoldable 
    
  indexed xs = Array.zipWith { x: _, i: _ } xs $ 0 .. (Array.length xs - 1)
  go Nil mm = mm
  go (group : rest) mm =
    case mm # windowed (Array.NonEmpty.length group) # Array.findIndex (Array.all isNothing) of
      Just idx | idx < (Array.NonEmpty.head group).i -> go rest $ moveGroup idx (List.fromFoldable group) mm
      _ -> go rest mm

  -- go (group : rest) mm =
  --   (0 .. Array.length mm) 
  --   # Array.find (\idx -> idx < (Array.NonEmpty.head group).i && (Array.slice idx (idx + Array.NonEmpty.length group) mm # Array.all isNothing)) 
  --   # case _ of
  --     Just idx | idx < (Array.NonEmpty.head group).i -> go rest $ moveGroup idx (List.fromFoldable group) mm
  --     _ -> go rest mm

  moveGroup _ Nil mm = mm
  moveGroup idx ({ i, x } : rest) mm =
    moveGroup (idx + 1) rest $ Array.updateAtIndices [ idx /\ x, i /\ join (Array.index mm idx) ] mm

windowed :: ∀ a. Int -> Array a -> Array (Array a)
windowed size xs =
  (0 .. (Array.length xs - 1))
    <#> (\i -> Array.slice i (i + size) xs)
    # Array.takeWhile (\ys -> Array.length ys == size)

solve :: Array MemoryCell -> Number
solve memMap =
  let
    rearranged = rearrange memMap
  in
    Array.zipWith (\i x -> x <#> (_ * i)) (0 .. (Array.length rearranged - 1)) rearranged
      # Array.catMaybes
      <#> Int.toNumber
      # sum

run :: Aff Unit
run = do
  contents <- getInputs
  let
    inputs = parseInput contents
    memMap = processMemoryMap inputs
    ans = solve memMap

  log ("Day 9, Puzzle 2: " <> show ans)
  -- 6415163624282
