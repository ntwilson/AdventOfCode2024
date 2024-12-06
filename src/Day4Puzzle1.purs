module Day4Puzzle1 where

import Prelude

import Data.Array as Array
import Data.String (Pattern(..))
import Data.String as String
import Effect.Aff (Aff)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile)


getInputs :: Aff (Array String)
getInputs = do
  contents <- readTextFile UTF8 "inputs/day4.txt"

  let 
    lines = String.split (Pattern "\n") contents
    nonEmptyLines = Array.filter (not <<< String.null) lines

  pure nonEmptyLines


