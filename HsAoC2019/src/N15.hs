{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}

module N15 (getSolutions15) where

import Control.Monad (filterM, foldM, when, unless)
import Control.Monad.Ref (MonadRef (readRef, writeRef), newRef)
import Data.Array ((!), (//))
import qualified Data.Array as A
import Data.Array.IO (IOArray, thaw)
import Data.Array.MArray (freeze, newArray, readArray, writeArray)
import Data.Foldable (forM_)
import Data.Functor (void)
import IntCode
import System.Console.ANSI
import Useful
import Data.Array.IO.Internals (IOArray(IOArray))
import Data.Array.Base (thawIOArray)

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
bounds = 25

explore :: [Int] -> CharGrid -> IO ()
explore code mazeMap = do
  robot <- createMachine @IOArray code
  let
    -- mazeMap = A.listArray ((-bounds, -bounds), (bounds, bounds)) [' ' | _ <- [-bounds .. bounds], _ <- [-bounds .. bounds]]
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
type DistMap = A.Array GridPos Distance 

findMaxDist  ::  GridPos -> A.Array GridPos Bool  -> Int 
findMaxDist  initPos walkableMap  = 
  let
    getWalkables :: GridPos -> [GridPos] -> [GridPos]
    getWalkables currentPos forbidden = [pos | dir <- [U, D, L, R], let pos = turn currentPos dir, pos `notElem` forbidden, walkableMap ! pos ] 
    go pos currentDist forbidden  = let  
      in case [go newPos (currentDist + 1) [pos]   | newPos <- getWalkables pos forbidden ] of 
        [] -> currentDist 
        ls -> maximum ls 
  in go initPos 0 [] 

autoExplore :: [Int] -> GridPos ->  IO (DistMap, Maybe GridPos)
autoExplore code initPos  = do
  robot <- createMachine @IOArray code
  distMap <-  newArray @IOArray ((-bounds, -bounds), (bounds, bounds)) Unknown
  writeArray distMap initPos (Dist 0)
  oxygenPosRef <- newRef Nothing
  let 
    walk :: Direction -> IO ()
    walk dir =  void $ runMachine [directionToInput dir] robot
    go currentPos currentDist breadcrumbs = do
       forM_ [U, D, L, R]
        ( \ dir -> do
            let pos = turn currentPos dir
            valAtPos <- readArray distMap pos
            when (valAtPos == Unknown) $  do
                output <- fmap head . getOutputs =<< runMachine [directionToInput dir] robot
                if output == 0 
                  then do
                    writeArray distMap pos Inf
                  else do
                    when (output == 2) (writeRef oxygenPosRef (Just pos)) 
                    writeArray distMap pos $ addDist currentDist 1
                    go pos (currentDist `addDist` 1) [dir]
        )
       mapM_ (walk . opposite) breadcrumbs

  go initPos (Dist 0)  []
  ar <- freeze distMap
  oxygenPos <- readRef oxygenPosRef
  return (ar, oxygenPos)


solution15  :: [Int] -> IO (Int, Int) --(A.Array GridPos Distance) 
solution15  code = do 
  (distMap, Just oxygen) <- autoExplore code (0,0)
  let walkableMap = (\case (Dist _) -> True ; _ -> False) <$> distMap 
      fillTime =  findMaxDist oxygen walkableMap 
  return (let Dist dist = distMap ! oxygen in dist , fillTime)

getSolutions15 filename = do 
 code <- codeParser <$> readFile filename
 solution15 code 

mapAndExplore :: [Int] -> IO ()
mapAndExplore code = do 
  (distMap, Just oxygen) <- autoExplore code (0,0) 
  let charGrid = (\case {Inf -> '#'; Unknown -> ' '; _ -> '.'}) <$> distMap 
      charGrid' = charGrid // [((0,0), 'S'), (oxygen, '!')]
  explore code charGrid'

