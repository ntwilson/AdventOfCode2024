module Day6Puzzle1 where

import Prelude

import Data.Array as Array
import Data.Foldable (indexl, sum)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
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
  | Visited
  | Obstacle
  | Guard Direction

derive instance Eq MapCoordinate
derive instance Generic MapCoordinate _
instance Show MapCoordinate where
  show = genericShow

type GeoMap = Array (Array MapCoordinate)

turn90DegreesRight :: Direction -> Direction
turn90DegreesRight Up = Right
turn90DegreesRight Right = Down
turn90DegreesRight Down = Left
turn90DegreesRight Left = Up

parseInput :: ∀ m. Monad m => Throws String m => Array String -> m GeoMap
parseInput input =
  input
    <#> CodeUnits.toCharArray
    # traverse
        ( traverse
            ( case _ of
                '.' -> pure Empty
                '#' -> pure Obstacle
                '^' -> pure $ Guard Up
                'v' -> pure $ Guard Down
                '<' -> pure $ Guard Left
                '>' -> pure $ Guard Right
                c -> throw ("Invalid character - " <> show c <> " - in input")
            )
        )

data GeoMapState
  = StillWalking GeoMap
  | LeftMap GeoMap

advanceOneStep :: ∀ m. Monad m => Throws String m => GeoMap -> m GeoMapState
advanceOneStep geoMap = do
  { guardDirection, guardLocation } <-
    geoMap
      # Array.mapWithIndex (\rowNum row -> row # Array.mapWithIndex (\colNum cell -> { rowNum, colNum, cell }))
      # Array.concat
      # Array.findMap
          ( \{ rowNum, colNum, cell } -> case cell of
              (Guard guardDirection) -> Just { guardDirection, guardLocation: { rowNum, colNum } }
              _ -> Nothing
          )
      # liftMaybe "No guard found in map"

  let
    nextStep = case guardDirection of
      Up -> { rowNum: guardLocation.rowNum - 1, colNum: guardLocation.colNum }
      Down -> { rowNum: guardLocation.rowNum + 1, colNum: guardLocation.colNum }
      Left -> { rowNum: guardLocation.rowNum, colNum: guardLocation.colNum - 1 }
      Right -> { rowNum: guardLocation.rowNum, colNum: guardLocation.colNum + 1 }

  case indexl nextStep.rowNum geoMap >>= indexl nextStep.colNum of
    Just Obstacle ->
      geoMap
        # Array.alterAt guardLocation.rowNum (Array.modifyAt guardLocation.colNum (const (Guard (turn90DegreesRight guardDirection))))
        # liftMaybe "Failed to find the guard's starting location"
        <#> StillWalking

    Just (Guard _) ->
      throw "Guard ran into another guard somehow"

    Nothing ->
      geoMap 
        # Array.alterAt guardLocation.rowNum (Array.modifyAt guardLocation.colNum (const Visited))
        # liftMaybe "Failed to find the guard's starting location"
        <#> LeftMap

    Just _ -> -- Empty or Visited again

      geoMap
        # Array.alterAt guardLocation.rowNum (Array.modifyAt guardLocation.colNum (const Visited))
        # liftMaybe "Failed to find the guard's starting location"
        >>=
          ( Array.alterAt nextStep.rowNum (Array.modifyAt nextStep.colNum (const (Guard guardDirection)))
              >>> liftMaybe "Failed to find the guard's next step"
          )
        <#> StillWalking


fullyExplore :: ∀ m. Monad m => Throws String m => GeoMap -> m GeoMap
fullyExplore geoMap = do
  advanceOneStep geoMap >>= case _ of
    LeftMap newMap -> pure newMap
    StillWalking newMap -> fullyExplore newMap

numVisited :: GeoMap -> Int
numVisited geoMap =
  geoMap
    <#> Array.filter
      ( case _ of
          Visited -> true
          _ -> false
      )
    <#> Array.length
    # sum

solve :: ∀ m. Monad m => Throws String m => GeoMap -> m Int
solve geoMap = 
  fullyExplore geoMap <#> numVisited

run :: Aff Unit
run = do
  inputs <- getInputs
  geoMap <- parseInput inputs
  ans <- solve geoMap

  log ("Day 6, Puzzle 1: " <> show ans)
