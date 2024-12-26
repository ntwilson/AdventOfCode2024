module Day8Puzzle2 where

import Prelude

import Control.Alternative as Alternative
import Data.Array as Array
import Data.Generic.Rep (class Generic)
import Data.List.Lazy as List
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

antinodes :: ∀ r. { width :: Int, height :: Int | r } -> Coordinate -> Coordinate -> Set Coordinate
antinodes geoMap a b =
  extendLine (List.iterate (_ + 1) 0) <> extendLine (List.iterate (_ - 1) 0)

  where

  extendLine is =
    is
      <#> (\i -> { row: a.row + dyReduced * i, col: a.col + dxReduced * i })
      # List.takeWhile (isCoordinateOnMap geoMap)
      # Set.fromFoldable

  dx = b.col - a.col
  dy = b.row - a.row

  gcf = gcd dx dy
  dxReduced = dx / gcf
  dyReduced = dy / gcf

allAntinodes :: ∀ r. { width :: Int, height :: Int | r } -> Set Coordinate -> Set Coordinate
allAntinodes geoMap antenna =
  pairs antenna
    # Set.map (\{ lesser, greater } -> antinodes geoMap lesser greater)
    # Set.unions

isCoordinateOnMap :: ∀ r. { width :: Int, height :: Int | r } -> Coordinate -> Boolean
isCoordinateOnMap { width, height } { row, col } =
  row >= 0 && row < height && col >= 0 && col < width

solve :: GeoMap -> Int
solve geoMap =
  geoMap.antennas
    # map (allAntinodes geoMap)
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

  log ("Day 8, Puzzle 2: " <> show ans)

