module Day1Puzzle2 where

import Prelude

import Data.Array as Array
import Data.Foldable (sum)
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Data.Ord (abs)
import Data.String (Pattern(..))
import Data.String as String
import Data.Tuple (Tuple)
import Data.Tuple.Nested ((/\))
import Effect.Aff (Aff)
import Effect.Class.Console (log)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile)

getInputs :: Aff (Array (Tuple Int Int))
getInputs = do
  contents <- readTextFile UTF8 "inputs/day1.txt"
  let
    lines = String.split (Pattern "\n") contents
    inputValues = String.split (Pattern "   ") <$> lines
    inputPairs = inputValues
      # Array.mapMaybe
          ( case _ of
              [ x, y ]
                | Just left <- Int.fromString x
                , Just right <- Int.fromString y -> Just (left /\ right)
              _ -> Nothing
          )
  pure inputPairs

solve :: Array (Tuple Int Int) -> _
solve contents =
  let
    (lefts /\ rights) = Array.unzip contents
    similarity =
      lefts
        <#>
          ( \l ->
              let
                count = Array.filter (_ == l) rights # Array.length
              in
                l * count
          )

  in
    sum similarity

run :: Aff Unit
run = do
  contents <- getInputs
  let
    ans = solve contents

  log ("Day 1, Puzzle 2: " <> show ans)

