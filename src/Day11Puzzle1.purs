module Day11Puzzle1 where

import Prelude

import Data.Array ((..))
import Data.Array as Array
import Data.Int as Int
import Data.Number as Number
import Data.Number.Format as Number
import Data.String (Pattern(..))
import Data.String as String
import Effect.Aff (Aff)
import Effect.Class.Console (log)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile)

getInputs :: Aff (Array Number)
getInputs = do
  contents <- readTextFile UTF8 "inputs/day11.txt"
  pure $ Array.mapMaybe Number.fromString $ Array.filter (not String.null) $ map String.trim $ String.split (Pattern " ") contents

advanceOneStone :: Number -> Array Number
advanceOneStone 0.0 = [1.0]
advanceOneStone stone | stoneStr <- Number.toString stone, String.length stoneStr `mod` 2 == 0 = 
  Array.mapMaybe Number.fromString [before, after] 
  where
  {before, after} = String.splitAt (String.length stoneStr `div` 2) stoneStr
advanceOneStone stone = [stone * 2024.0]

advance :: Array Number -> Array Number
advance = Array.concatMap advanceOneStone

advanceNTimes :: Int -> Array Number -> Array Number
advanceNTimes n stones = Array.foldl (\acc _ -> advance acc) stones (1 .. n)

solve :: Array Number -> Number
solve stones = advanceNTimes 25 stones # Array.length # Int.toNumber

run :: Aff Unit
run = do
  contents <- getInputs
  let
    ans = solve contents

  log ("Day 11, Puzzle 1: " <> show ans)

