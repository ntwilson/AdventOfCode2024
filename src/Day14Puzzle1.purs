module Day14Puzzle1 where

import Prelude

import Data.Array ((..))
import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.String (Pattern(..))
import Data.String as String
import Data.Traversable (traverse)
import Effect.Aff (Aff)
import Effect.Class.Console (log)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile)
import Parsing (parseErrorMessage, runParser)
import Parsing.String as Parsing
import Parsing.String.Basic as Parsing
import Throws (class Throws, liftEither)

getInputs :: Aff (Array String)
getInputs = do
  contents <- readTextFile UTF8 "inputs/day14.txt"
  pure $ Array.filter (not String.null) $ String.split (Pattern "\n") contents

type Cartesian = { x :: Int, y :: Int }
type Robot = { position :: Cartesian, velocity :: Cartesian }

parseInputs :: ∀ m. Applicative m => Throws String m => Array String -> m (Array Robot)
parseInputs inputs = traverse parseRobot inputs

  where
  parseRobot s = liftEither $ lmap parseErrorMessage $ runParser s do
    position <- parsePosition
    _ <- Parsing.skipSpaces
    velocity <- parseVelocity
    pure { position, velocity }

  parsePosition = do
    _ <- Parsing.string "p="
    x <- Parsing.intDecimal
    _ <- Parsing.string ","
    y <- Parsing.intDecimal
    pure { x, y }

  parseVelocity = do
    _ <- Parsing.string "v="
    x <- Parsing.intDecimal
    _ <- Parsing.string ","
    y <- Parsing.intDecimal
    pure { x, y }

type Bounds = { width :: Int, height :: Int }

moveRobot :: { width :: Int, height :: Int } -> Robot -> Robot
moveRobot { width, height } robot =
  robot { position = newPosition }

  where
  { x: px, y: py } = robot.position
  { x: vx, y: vy } = robot.velocity
  x = if (px + vx) < 0 then width + px + vx else (px + vx) `mod` width
  y = if (py + vy) < 0 then height + py + vy else (py + vy) `mod` height

  newPosition = { x, y }

moveAllRobots :: Bounds -> Int -> Array Robot -> Array Robot
moveAllRobots bounds nTimes robots =  
  robots 
  # map (\robot -> (1 .. nTimes) # Array.foldl (\r _ -> moveRobot bounds r) robot)

partitionQuadrants :: Bounds -> Array Robot -> { q1 :: (Array Robot), q2 :: (Array Robot), q3 :: (Array Robot), q4 :: (Array Robot) }
partitionQuadrants {width, height} robots = { q1, q2, q3, q4 }
  where
  compareX = if width `mod` 2 == 0 then (>=) else (>)
  compareY = if height `mod` 2 == 0 then (>=) else (>)
  
  q1 =
    robots 
    # Array.filter (\{position: {x, y}} -> x < width `div` 2 && y < height `div` 2) 

  q2 =
    robots 
    # Array.filter (\{position: {x, y}} -> (x `compareX` (width `div` 2)) && y < height `div` 2) 

  q3 =
    robots 
    # Array.filter (\{position: {x, y}} -> x < width `div` 2 && (y `compareY` (height `div` 2)))

  q4 =
    robots 
    # Array.filter (\{position: {x, y}} -> (x `compareX` (width `div` 2)) && (y `compareY` (height `div` 2)))

scoreQuadrants :: Bounds -> Array Robot -> Int
scoreQuadrants bounds robots =
  nq1 * nq2 * nq3 * nq4

  where
  { q1, q2, q3, q4 } = partitionQuadrants bounds robots 

  nq1 = Array.length q1
  nq2 = Array.length q2
  nq3 = Array.length q3
  nq4 = Array.length q4


solve :: Bounds -> Array Robot -> Int
solve bounds robots = 
  scoreQuadrants bounds $ moveAllRobots bounds 100 robots 

run :: Aff Unit
run = do
  contents <- getInputs
  input <- parseInputs contents
  let
    ans = solve { height: 103, width: 101 } input

  log ("Day 14, Puzzle 1: " <> show ans)

