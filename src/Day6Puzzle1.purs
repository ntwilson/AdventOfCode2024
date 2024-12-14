module Day6Puzzle1 where

import Prelude

import Data.Array as Array
import Data.Foldable (indexl)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Set (Set)
import Data.Set as Set
import Data.Show.Generic (genericShow)
import Data.String (Pattern(..))
import Data.String as String
import Data.String.CodeUnits as CodeUnits
import Data.Traversable (traverse)
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

data MapCoordinate
  = Empty
  | Obstacle

derive instance Eq MapCoordinate
derive instance Generic MapCoordinate _
instance Show MapCoordinate where show = genericShow


type GeoMap = 
  { geoMap :: Array (Array MapCoordinate)
  , guard :: { row :: Int, col :: Int, direction :: Direction }
  , visitations :: Set { row :: Int, col :: Int }
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

  visitations = Set.empty
  
  guardResult = 
    input
      <#> CodeUnits.toCharArray
      # Array.mapWithIndex (\row ->
        Array.mapWithIndex (\col ->
          case _ of
            '^' -> Just { row, col, direction: Up }
            'v' -> Just { row, col, direction: Down }
            '<' -> Just { row, col, direction: Left }
            '>' -> Just { row, col, direction: Right }
            _ -> Nothing
        )
      )
      # Array.concat
      # Array.catMaybes
      # Array.head
      # liftMaybe "Failed to find the guard's starting location"

data GeoMapState
  = StillWalking GeoMap
  | LeftMap GeoMap

advanceOneStep :: GeoMap -> GeoMapState
advanceOneStep totalMap@{ guard, visitations, geoMap } =
  let
    nextStep = case guard.direction of
      Up -> { rowNum: guard.row - 1, colNum: guard.col }
      Down -> { rowNum: guard.row + 1, colNum: guard.col }
      Left -> { rowNum: guard.row, colNum: guard.col - 1 }
      Right -> { rowNum: guard.row, colNum: guard.col + 1 }

  in case indexl nextStep.rowNum geoMap >>= indexl nextStep.colNum of
    Just Obstacle ->
      totalMap 
        { guard = guard { direction = turn90DegreesRight guard.direction } }
        # StillWalking

    Nothing ->
      LeftMap totalMap

    Just _ -> -- Empty or Visited again
      totalMap
        { guard = guard { row = nextStep.rowNum, col = nextStep.colNum } 
        , visitations = visitations # Set.insert {row: nextStep.rowNum, col: nextStep.colNum}
        }
      # StillWalking


fullyExplore :: GeoMap -> GeoMap
fullyExplore geoMap =
  case advanceOneStep geoMap of
    LeftMap newMap -> newMap
    StillWalking newMap -> fullyExplore newMap

numVisited :: GeoMap -> Int
numVisited {visitations} = Set.size visitations

solve :: GeoMap -> Int
solve geoMap = 
  fullyExplore geoMap # numVisited

run :: Aff Unit
run = do
  inputs <- getInputs
  geoMap <- parseInput inputs
  let
    ans = solve geoMap

  log ("Day 6, Puzzle 1: " <> show ans)
