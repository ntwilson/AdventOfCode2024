module Day3Puzzle1 where

import Prelude

import Effect.Aff (Aff)
import Effect.Class.Console (log)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile)

getInputs :: Aff String
getInputs = do
  readTextFile UTF8 "inputs/day3.txt"

solve :: String -> _
solve _inputStr =
  0

run :: Aff Unit
run = do
  contents <- getInputs
  let
    ans = solve contents

  log ("Day 3, Puzzle 1: " <> show ans)

