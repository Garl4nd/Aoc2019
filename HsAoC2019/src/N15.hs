{-# LANGUAGE TypeApplications #-}

module N15 () where

import Control.Monad (when)
import Data.Array ((!), (//))
import qualified Data.Array as A
import Data.Array.IO (IOArray)
import IntCode
import System.Console.ANSI
import Useful

charToDirection :: Char -> Maybe Direction
charToDirection = \case
  'w' -> Just U
  's' -> Just D
  'a' -> Just L
  'd' -> Just R
  _ -> Nothing

directionToInput :: Direction -> Int
directionToInput = \case
  U -> 1
  D -> 2
  L -> 3
  R -> 4

data Distance = Dist Int | Inf deriving (Show, Eq)
addDist :: Distance -> Int -> Distance
addDist (Dist c) d = Dist (c + d)
addDist _ _ = Inf

data Direction = U | D | L | R deriving (Show)
turn :: GridPos -> Direction -> GridPos
turn (y, x) U = (y - 1, x)
turn (y, x) D = (y + 1, x)
turn (y, x) L = (y, x - 1)
turn (y, x) R = (y, x + 1)

outputToChar :: Int -> Char
outputToChar 0 = '#'
outputToChar 1 = '.'
outputToChar 2 = '!'

bounds :: Int
bounds = 35
explore :: [Int] -> IO ()
explore code = do
  robot <- createMachine @IOArray code
  let
    mazeMap = A.listArray ((-bounds, -bounds), (bounds, bounds)) [' ' | _ <- [-bounds .. bounds], _ <- [-bounds .. bounds]]
    distMap = (Inf <$ mazeMap) // [((0, 0), Dist 0)]
    go pos mazeMap currentDist distMap = do
      clearScreen
      print (pos, currentDist)
      putStrLn $ unlines . charGridToStr $ mazeMap // [(pos, 'R')]
      directionChar <- getChar
      let direction = charToDirection directionChar
      case direction of
        Just dir -> do
          let input = directionToInput dir
              desiredPosition = turn pos dir
          outputs <- getOutputs =<< runMachine [input] robot
          when (null outputs) $ go pos mazeMap currentDist distMap
          let outputCode = head outputs
              updatedMap = mazeMap // [(desiredPosition, outputToChar outputCode)]
              (updatedPos, updatedDist, updatedDistMap) =
                if
                  | outputCode == 0 -> (pos, currentDist, distMap)
                  | distMap ! desiredPosition == Inf -> (desiredPosition, currentDist `addDist` 1, distMap // [(desiredPosition, currentDist `addDist` 1)])
                  | otherwise -> (desiredPosition, distMap ! desiredPosition, distMap)
          go updatedPos updatedMap updatedDist updatedDistMap
        Nothing -> go pos mazeMap currentDist distMap
  go (0, 0) mazeMap (Dist 0) distMap
