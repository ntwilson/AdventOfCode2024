module Day10Puzzle1 where

import Prelude

import Data.Array ((!!))
import Data.Array as Array
import Data.Foldable (sum)
import Data.Int as Int
import Data.Maybe (Maybe)
import Data.Set as Set
import Data.String (Pattern(..))
import Data.String as String
import Data.String.CodeUnits (singleton)
import Data.String.CodeUnits as String
import Effect.Aff (Aff)
import Effect.Class.Console (log)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile)

getInputs :: Aff (Array String)
getInputs = do
  contents <- readTextFile UTF8 "inputs/day10.txt"
  pure $ Array.filter (not String.null) $ String.split (Pattern "\n") contents

type Cell = { row :: Int, col :: Int, height :: Int }
type GeoMap = Array (Array Cell)

lookup :: ∀ a. Int -> Int -> Array (Array a) -> Maybe a
lookup row col grid = do
  grid !! row >>= (_ !! col)

parseInput :: Array String -> GeoMap
parseInput contents =
  contents
    <#> String.toCharArray
    <#> Array.mapMaybe (Int.fromString <<< singleton)
    # Array.mapWithIndex (\row cells -> 
      cells 
      # Array.mapWithIndex (\col height -> { row, col, height }))

score :: GeoMap -> Cell -> Int
score geoMap trailhead = Set.size $ Set.fromFoldable $ endLocations [trailhead]
  where
  endLocations locs = do 
    loc <- locs
    if loc.height == 9
      then [loc]
      else  
        let
          down = geoMap # lookup (loc.row+1) loc.col
          up = geoMap # lookup (loc.row-1) loc.col
          left = geoMap # lookup loc.row (loc.col-1)
          right = geoMap # lookup loc.row (loc.col+1)
        in endLocations ([down,up,left,right] # Array.catMaybes # Array.filter (\{height} -> height == loc.height + 1))


solve :: GeoMap -> Int
solve geoMap =
  trailheads <#> score geoMap # sum

  where
  trailheads = 
    geoMap
    # Array.concatMap (Array.filter (\{height} -> height == 0))


run :: Aff Unit
run = do
  contents <- getInputs
  let
    inputs = parseInput contents
    ans = solve inputs

  log ("Day 10, Puzzle 1: " <> show ans)

