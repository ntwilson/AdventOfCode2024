module Day11Puzzle2 where

import Prelude

import Data.Array ((..))
import Data.Array as Array
import Data.Foldable (sum)
import Data.Int as Int
import Data.Lazy (defer, force)
import Data.Maybe (Maybe(..))
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
advanceOneStone 0.0 = [ 1.0 ]
advanceOneStone stone | stoneStr <- Number.toString stone, String.length stoneStr `mod` 2 == 0 =
  Array.mapMaybe Number.fromString [ before, after ]
  where
  { before, after } = String.splitAt (String.length stoneStr `div` 2) stoneStr
advanceOneStone stone = [ stone * 2024.0 ]

memos :: _
memos =
  (0 .. 75999)
    <#> (\combined -> 
      let 
        nEvals = combined / 1000
        rock = combined `mod` 1000
      in 
        defer (\_ -> advanceOneStone (Int.toNumber rock) <#> evalMemo (nEvals - 1) # sum))

evalMemo :: Int -> Number -> Number
evalMemo 1 rock = advanceOneStone rock # Array.length # Int.toNumber
evalMemo nEvals rockNum 
  | Just rockInt <- Int.fromNumber rockNum
  , rockInt < 1000 
  , Just lazy <- Array.index memos (nEvals * 1000 + rockInt) = force lazy
evalMemo nEvals rock = advanceOneStone rock <#> evalMemo (nEvals - 1) # sum

solve :: Array Number -> Number
solve stones = stones <#> evalMemo 75 # sum

run :: Aff Unit
run = do
  contents <- getInputs
  let
    ans = solve contents

  log ("Day 11, Puzzle 2: " <> show ans)

