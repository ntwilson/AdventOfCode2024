module Day8Puzzle1 where

import Prelude

import Control.Alternative as Alternative
import Data.Array as Array
import Data.Generic.Rep (class Generic)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Set (Set)
import Data.Set as Set
import Data.String (Pattern(..))
import Data.String as String
import Data.String.CodeUnits as CodeUnits
import Data.Tuple.Nested ((/\))
import Effect.Aff (Aff)
import Effect.Class.Console (log)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile)

getInputs :: Aff (Array String)
getInputs = do
  contents <- readTextFile UTF8 "inputs/day8.txt"
  pure $ Array.filter (not String.null) $ String.split (Pattern "\n") contents

type Antenna = Char
data MapCoordinate
  = Empty
  | Antenna Antenna

derive instance Eq MapCoordinate
derive instance Generic MapCoordinate _
instance Show MapCoordinate where
  show Empty = "."
  show (Antenna c) = show c

type Coordinate = { row :: Int, col :: Int }
type GeoMap = { width :: Int, height :: Int, antennas :: Map Antenna (Set Coordinate) }

parseInput :: Array String -> Array (Array MapCoordinate)
parseInput input =
  input
    <#> CodeUnits.toCharArray
    # map
        ( map
            ( case _ of
                '.' -> Empty
                c -> Antenna c
            )
        )

processMap :: Array (Array MapCoordinate) -> GeoMap
processMap geoMap = { width, height, antennas }
  where
  width = geoMap # Array.head <#> Array.length # fromMaybe 0
  height = Array.length geoMap
  antennas =
    geoMap
      # Array.mapWithIndex
          ( \row ->
              Array.mapWithIndex
                ( \col cell -> case cell of
                    Empty -> Nothing
                    Antenna c -> Just (c /\ Set.singleton { row, col })
                )
          )
      # Array.concat
      # Array.catMaybes
      # Map.fromFoldableWith (<>)

pairs :: ∀ a. Ord a => Set a -> Set { lesser :: a, greater :: a }
pairs set = Set.fromFoldable do
  left <- Array.fromFoldable set
  right <- Array.fromFoldable set
  Alternative.guard (left /= right)
  let
    lesser = min left right
    greater = max left right
  pure { lesser, greater }

antinodes :: Coordinate -> Coordinate -> Set Coordinate
antinodes a b =
  Set.fromFoldable [ left, right ]

  where
  left =
    { row: a.row + (b.row - a.row) * 2
    , col: a.col + (b.col - a.col) * 2
    }

  right =
    { row: b.row + (a.row - b.row) * 2
    , col: b.col + (a.col - b.col) * 2
    }

allAntinodes :: Set Coordinate -> Set Coordinate
allAntinodes antenna =
  pairs antenna
    # Set.map (\{ lesser, greater } -> antinodes lesser greater)
    # Set.unions

isCoordinateOnMap :: ∀ r. { width :: Int, height :: Int | r } -> Coordinate -> Boolean
isCoordinateOnMap { width, height } { row, col } =
  row >= 0 && row < height && col >= 0 && col < width

solve :: GeoMap -> Int
solve geoMap =
  geoMap.antennas
    # map (allAntinodes >>> Set.filter (isCoordinateOnMap geoMap))
    # Map.values
    # Set.unions
    # Set.size

run :: Aff Unit
run = do
  contents <- getInputs
  let
    inputs = parseInput contents
    geoMap = processMap inputs
    ans = solve geoMap

  log ("Day 8, Puzzle 1: " <> show ans)

