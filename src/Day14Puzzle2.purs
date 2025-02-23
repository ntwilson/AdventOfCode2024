module Day14Puzzle2 where

import Prelude

import Control.Monad.Cont (lift)
import Control.Monad.State (StateT, evalStateT)
import Control.Monad.State as State
import Control.Promise (Promise, toAff)
import Data.Array ((..))
import Data.Array as Array
import Data.Bifunctor (lmap)
-- import Data.List.Lazy as List
import Data.String (Pattern(..))
import Data.String as String
import Data.String.CodeUnits as String
import Data.Traversable (for_, traverse)
import Effect.Aff (Aff)
import Effect.Aff.Compat (EffectFn1, runEffectFn1)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile)
import Parsing (parseErrorMessage, runParser)
import Parsing.String as Parsing
import Parsing.String.Basic as Parsing
import Throws (class Throws, liftEither)

foreign import questionImpl :: EffectFn1 String (Promise String)

question :: String -> Aff String
question txt = liftEffect (runEffectFn1 questionImpl txt) >>= toAff

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

drawMap :: Bounds -> Array Robot -> String
drawMap { width, height } robots =
  (0 .. (height - 1))
  <#> (\row -> 
    (0 .. (width - 1))
    <#> (\col ->
      if Array.any (\{position: {x, y}} -> x == col && y == row) robots
      then '█'
      else ' '
    )
    # String.fromCharArray
  )
  # String.joinWith "\n"

run :: Aff Unit
run = do
  contents <- getInputs
  input <- parseInputs contents
  log "Day 14, Puzzle 2: "
  let 
    bounds = { height: 103, width: 101 }
    ans = moveAllRobots bounds 8050 input

  log "  After 8050 seconds:"
  log (drawMap bounds ans)
   
  
findAns :: Aff Unit
findAns = do
  contents <- getInputs
  input <- parseInputs contents
  log "Day 14, Puzzle 2: "
  let 
    bounds = { height: 103, width: 101 }
    initialState = moveAllRobots bounds 8049 input
    stateProg :: StateT (Array Robot) Aff Unit
    stateProg = 
      for_ (8050 .. 10000) (\i -> do
        log ("  After " <> (show i) <> " seconds: ")
        robots <- State.modify (moveAllRobots bounds 1)
        log (drawMap bounds robots)
        void $ lift $ question "Press Enter to continue")

  evalStateT stateProg initialState


