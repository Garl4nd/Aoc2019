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
bounds = 40
explore :: [Int] -> IO ()
explore code = go (0, 0) $ A.listArray ((-bounds, -bounds), (bounds, bounds)) ['?' | _ <- [-bounds .. bounds], _ <- [-bounds .. bounds]]
 where
  go pos mazeMap = do
    clearScreen
    putStrLn $ unlines . charGridToStr $ mazeMap
    robot <- createMachine @IOArray code
    directionChar <- getChar
    let direction = charToDirection directionChar
    case direction of
      Just dir -> do
        let input = directionToInput dir
            desiredPosition = turn pos dir
        outputs <- getOutputs =<< runMachine [input] robot
        when (null outputs) $ go pos mazeMap
        let outputCode = head outputs
            updatedMap = mazeMap // [(desiredPosition, outputToChar outputCode)]
            updatedPos = if outputCode == 0 then pos else desiredPosition
        go updatedPos updatedMap
      Nothing -> go pos mazeMap
