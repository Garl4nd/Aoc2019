{-# LANGUAGE TypeApplications #-}

module N13 () where

import Control.Monad (void, when)
import Data.Array ((!), (//))
import qualified Data.Array as A
import Data.Array.IO (IOArray)
import Data.Char (ord)
import Data.Function (fix)
import Data.List (partition)
import IntCode
import Useful

solution1 :: [Int] -> Int
solution1 code =
  let
    output = machineOutputs $ runCodeWInputST [] code
    objects = chunksOf 3 output
    tilePos = [(x, y) | [x, y, objId] <- objects, objId == 2]
   in
    length tilePos

objIdToChar :: Int -> Char
objIdToChar = \case
  0 -> ' '
  1 -> 'x'
  2 -> '!'
  3 -> 'T'
  4 -> 'O'
  _ -> error "Wrong id"

updateGameAr :: [[Int]] -> CharGrid -> CharGrid
updateGameAr updates grid =
  grid // [((y, x), objIdToChar objId) | [x, y, objId] <- updates]
getSolutions13 = getSolutions codeParser solution1 (const 0)
dim :: Int
dim = 45
autoMode = True 
playGame :: [Int] -> IO ()
playGame code = do
  machine <- createMachine @IOArray (2 : tail code)
  let gameAr = A.listArray ((0, 0), (dim, dim)) [' ' | _ <- [0 .. dim], _ <- [0 .. dim]]
  
  flip fix (gameAr, 0, 0) $ \loop (currentAr, autoInput, score) -> do
    userInput <-  getChar
    let input = if userInput == 'a' then autoInput else subtract 107 . ord $ userInput 
    when (input == -4) $ playGame code
    when (input < -1 || input > 1) $ do
      -- putStrLn "Invalid input!"
      loop (currentAr, 0, score)
    -- print input
    
    outputs <- getOutputs =<< runMachine [input] machine
    when (null outputs) $ loop (currentAr, 0, score)
    -- print outputs
    let (resList, updates) = partition (\[x, y, _] -> x == -1 && y == 0) $ chunksOf 3 outputs
    let platformPos = case [x | [x,_,3] <- updates] of [] -> 0; f:_ -> f
        ballPos = case  [x | [x,_,4] <- updates] of [] -> 0; f:_ -> f
        updatedScore =  case resList of
            [] -> score 
            res : _ -> let [_, _, newScore] = res in newScore  
    putStrLn $ "score = " <> show updatedScore
    let newAr = updateGameAr updates currentAr
    putStrLn $ unlines . charGridToStr $ newAr
    loop (newAr, if ballPos > platformPos then 1 else -1, updatedScore)
