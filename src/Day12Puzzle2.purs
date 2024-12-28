module Day12Puzzle2 where

import Prelude

import Control.Monad.State (State, evalState, execState, get, modify, put)
import Data.Array ((!!))
import Data.Array as Array
import Data.Foldable (sum)
import Data.Maybe (Maybe(..))
import Data.Set (Set)
import Data.Set as Set
import Data.String (Pattern(..))
import Data.String as String
import Data.String.CodeUnits as String
import Data.TraversableWithIndex (traverseWithIndex)
import Effect.Aff (Aff)
import Effect.Class.Console (log)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile)

getInputs :: Aff (Array (Array Char))
getInputs = do
  contents <- readTextFile UTF8 "inputs/day12.txt"
  pure $ map String.toCharArray $ Array.filter (not String.null) $ String.split (Pattern "\n") contents

type Coordinate = { row :: Int, col :: Int }

lookup :: ∀ a. Int -> Int -> Array (Array a) -> Maybe a
lookup row col grid = do
  grid !! row >>= (_ !! col)

regionFor :: Array (Array Char) -> Coordinate -> Char -> Array Coordinate
regionFor geoMap coord cell = Array.fromFoldable $ execState (go coord) Set.empty
  where
    go :: Coordinate -> State (Set Coordinate) Unit
    go xy = do
      let
        down = xy { row = xy.row + 1 }
        right = xy { col = xy.col + 1 }
        left = xy { col = xy.col - 1 }
        up = xy { row = xy.row - 1 }
      
      coordinatesProcessed <- get
      if (lookup xy.row xy.col geoMap /= Just cell) || Set.member xy coordinatesProcessed 
      then pure unit
      else do
        _ <- modify (Set.insert xy)
        go down
        go right
        go left
        go up


regions :: Array (Array Char) -> Array (Array Coordinate)
regions geoMap =
  geoMap
    # traverseWithIndex
        ( \row cells ->
            cells
              # traverseWithIndex
                  ( \col cell -> do
                      coordinatesProcessed <- get
                      let coord = { row, col }
                      if Set.member coord coordinatesProcessed then pure Nothing
                      else do
                        let region = regionFor geoMap coord cell
                        put (coordinatesProcessed <> (Set.fromFoldable region))
                        pure (Just region)
                  )
        )
    # flip evalState Set.empty
    # Array.concatMap (Array.catMaybes)

areaOfRegion :: Array Coordinate -> Int
areaOfRegion = Array.length

nSidesOfRegion :: Array Coordinate -> Int
nSidesOfRegion coords = Array.length allSides
  where
  up coord = coord { row = coord.row - 1 }
  left coord = coord { col = coord.col - 1 }
  right coord = coord { col = coord.col + 1 }
  down coord = coord { row = coord.row + 1 }

  perimeter direction = do
    coord <- coords
    [ direction coord ]
      # Array.filter (\y -> not (y `Array.elem` coords))

  sides perim = 
    perim # Array.filter (\y -> 
      let 
        above = y { row = y.row - 1}
        toTheLeft = y { col = y.col - 1}
      in 
        not (above `Array.elem` perim) && not (toTheLeft `Array.elem` perim))

  allSides = 
    [ up, left, right, down ]
      # Array.concatMap (perimeter >>> sides)

priceOfRegion :: Array Coordinate -> Int
priceOfRegion region = areaOfRegion region * nSidesOfRegion region

solve :: Array (Array Char) -> Int
solve input = 
  regions input
    # map priceOfRegion
    # sum

run :: Aff Unit
run = do
  contents <- getInputs
  let
    ans = solve contents

  log ("Day 12, Puzzle 2: " <> show ans)

