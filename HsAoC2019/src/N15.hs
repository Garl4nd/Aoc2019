{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}

module N15 () where

import Control.Monad (filterM, foldM, when)
import Control.Monad.Ref (MonadRef (readRef, writeRef), newRef)
import Data.Array ((!), (//))
import qualified Data.Array as A
import Data.Array.IO (IOArray)
import Data.Array.MArray (freeze, newArray, readArray, writeArray)
import Data.Foldable (forM_)
import Data.Functor (void)
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
opposite :: Direction -> Direction
opposite D = U
opposite U = D
opposite L = R
opposite R = L

data Distance = Dist Int | Inf | Unknown deriving (Show, Eq)
addDist :: Distance -> Int -> Distance
addDist (Dist c) d = Dist (c + d)
addDist Inf _ = Inf
addDist Unknown _ = Inf

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

autoExplore :: [Int] -> IO (A.Array GridPos Distance, Maybe GridPos)
autoExplore code = do
  robot <- createMachine @IOArray code
  distMap <- newArray @IOArray ((-bounds, -bounds), (bounds, bounds)) Unknown
  writeArray distMap (0, 0) (Dist 0)
  oxygenPosRef <- newRef Nothing
  let
    updateDistMapAndGetWalkables :: GridPos -> Distance -> IO [(GridPos, Direction)]
    updateDistMapAndGetWalkables currentPos currentDist =
      foldM
        ( \walkableDirections dir -> do
            let pos = turn currentPos dir
            valAtPos <- readArray distMap pos
            -- print (pos, valAtPos)
            case valAtPos of
              Unknown -> do
                output <- fmap head . getOutputs =<< runMachine [directionToInput dir] robot
                if output == 0
                  then do
                    writeArray distMap pos Inf
                    return walkableDirections
                  else do
                    _ <- runMachine [directionToInput $ opposite dir] robot
                    when (output == 2) $ writeRef oxygenPosRef $ Just pos
                    writeArray distMap pos $ addDist currentDist 1
                    return $ (pos, dir) : walkableDirections
              _ -> return walkableDirections
        )
        []
        [U, D, L, R]
    walk :: Direction -> IO ()
    walk dir = void $ runMachine [directionToInput dir] robot
    goBack = mapM_ (walk . opposite)
    go pos currentDist initialPos breadcrumbs = do
      walkables <- updateDistMapAndGetWalkables pos currentDist
      print walkables
      case walkables of
        [] -> goBack breadcrumbs
        [(newPos, newDir)] -> walk newDir >> go newPos (addDist currentDist 1) initialPos (newDir : breadcrumbs)
        _ -> sequence_ [walk newDir >> go newPos (addDist currentDist 1) pos [newDir] | (newPos, newDir) <- walkables]

  go (0, 0) (Dist 0) (0, 0) []
  ar <- freeze distMap
  oxygenPos <- readRef oxygenPosRef
  return (ar, oxygenPos)
