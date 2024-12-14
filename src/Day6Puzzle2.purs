module Day6Puzzle2 where

import Prelude

import Data.Array as Array
import Data.Either (Either)
import Data.Either as Either
import Data.Generic.Rep (class Generic)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Set (Set)
import Data.Show.Generic (genericShow)
import Data.String (Pattern(..))
import Data.String as String
import Data.String.CodeUnits as CodeUnits
import Data.Traversable (traverse)
import Data.Tuple.Nested ((/\))
import Effect.Aff (Aff)
import Effect.Class.Console (log)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile)
import Throws (class Throws, throw, liftMaybe)

getInputs :: Aff (Array String)
getInputs = do
  contents <- readTextFile UTF8 "inputs/day6.txt"
  pure $ String.split (Pattern "\n") contents

data Direction = Up | Down | Left | Right

derive instance Eq Direction
derive instance Generic Direction _
instance Show Direction where
  show = genericShow

derive instance Ord Direction

data MapCoordinate
  = Empty
  | Obstacle

derive instance Eq MapCoordinate
derive instance Generic MapCoordinate _
instance Show MapCoordinate where
  show = genericShow

type GeoMap =
  { geoMap :: Map { row :: Int, col :: Int } MapCoordinate
  , guard :: { coord :: { row :: Int, col :: Int }, direction :: Direction }
  , visitations :: Map { row :: Int, col :: Int } (Array Direction)
  }

turn90DegreesRight :: Direction -> Direction
turn90DegreesRight Up = Right
turn90DegreesRight Right = Down
turn90DegreesRight Down = Left
turn90DegreesRight Left = Up

parseInput :: ∀ m. Monad m => Throws String m => Array String -> m GeoMap
parseInput input = do
  geoMap <- geoMapResult
  guard <- guardResult
  pure { geoMap, guard, visitations }

  where
  geoMapResult =
    input
      <#> CodeUnits.toCharArray
      # traverse
          ( traverse
              ( case _ of
                  '#' -> pure Obstacle
                  '.' -> pure Empty
                  '^' -> pure Empty
                  'v' -> pure Empty
                  '<' -> pure Empty
                  '>' -> pure Empty
                  c -> throw ("Invalid character - " <> show c <> " - in input")
              )
          )
      <#> Array.mapWithIndex
        ( \row ->
            Array.mapWithIndex (\col cell -> { row, col } /\ cell)
        )
      <#> Array.concat
      <#> Map.fromFoldable

  visitations = Map.empty

  guardResult =
    input
      <#> CodeUnits.toCharArray
      # Array.mapWithIndex
          ( \row ->
              Array.mapWithIndex
                ( \col ->
                    case _ of
                      '^' -> Just { coord: { row, col }, direction: Up }
                      'v' -> Just { coord: { row, col }, direction: Down }
                      '<' -> Just { coord: { row, col }, direction: Left }
                      '>' -> Just { coord: { row, col }, direction: Right }
                      _ -> Nothing
                )
          )
      # Array.concat
      # Array.catMaybes
      # Array.head
      # liftMaybe "Failed to find the guard's starting location"

data GeoMapEndState
  = LeftMap
  | StuckInLoop

derive instance Eq GeoMapEndState
derive instance Generic GeoMapEndState _
instance Show GeoMapEndState where
  show = genericShow

advanceOneStep :: GeoMap -> Either GeoMap { endState :: GeoMapEndState, visitations :: Set { row :: Int, col :: Int } }
advanceOneStep totalMap@{ guard, visitations, geoMap } =
  let
    nextStep = case guard.direction of
      Up -> { row: guard.coord.row - 1, col: guard.coord.col }
      Down -> { row: guard.coord.row + 1, col: guard.coord.col }
      Left -> { row: guard.coord.row, col: guard.coord.col - 1 }
      Right -> { row: guard.coord.row, col: guard.coord.col + 1 }

  in
    case Map.lookup nextStep geoMap of
      Just Obstacle ->
        totalMap
          { guard = guard { direction = turn90DegreesRight guard.direction } }
          # Either.Left

      Nothing ->
        Either.Right $ { endState: LeftMap, visitations: Map.keys visitations }

      Just Empty
        | Just directions <- visitations # Map.lookup nextStep
        , Array.elem guard.direction directions ->
            Either.Right $ { endState: StuckInLoop, visitations: Map.keys visitations }

      Just Empty ->
        totalMap
          { guard = guard { coord = nextStep }
          , visitations =
              visitations
                # Map.alter (\directions -> Just (Array.cons guard.direction $ fromMaybe [] directions)) nextStep
          }
          # Either.Left

fullyExplore :: GeoMap -> { endState :: GeoMapEndState, visitations :: Set { row :: Int, col :: Int } }
fullyExplore geoMap =
  case advanceOneStep geoMap of
    Either.Right endState -> endState
    Either.Left newMap -> fullyExplore newMap

permutations :: GeoMap -> Array GeoMap
permutations geoMap =
  (fullyExplore geoMap).visitations
    # Array.fromFoldable
    # Array.mapMaybe
        ( \{ row, col } ->
            case Map.lookup { row, col } geoMap.geoMap of
              Just Empty | { row, col } /= geoMap.guard.coord -> Just geoMap { geoMap = geoMap.geoMap # Map.insert { row, col } Obstacle }
              _ -> Nothing
        )

solve :: GeoMap -> Int
solve geoMap =
  permutations geoMap
    <#> fullyExplore
    # Array.filter
        ( case _ of
            { endState: StuckInLoop } -> true
            _ -> false
        )
    # Array.length

run :: Aff Unit
run = do
  inputs <- getInputs
  geoMap <- parseInput inputs
  let
    ans = solve geoMap

  log ("Day 6, Puzzle 2: " <> show ans)

