{-# LANGUAGE FlexibleContexts #-}

module N7 () where

import Control.Monad (foldM)
import Control.Monad.Ref (readRef)
import Control.Monad.ST
import Data.Array.IO (IOArray)
import Data.Array.ST (STArray)
import Data.IORef (IORef)
import Data.List (foldl', permutations)
import Data.STRef (STRef)
import Debugging (traceWInfo)
import IntCode

import Control.Monad.Ref
import Data.Array.Base (MArray)
import Useful (getSolutions)

midEl (_, x, _) = x

runAmplifiers :: [Int] -> [Int] -> Int
runAmplifiers code settings = foldl' (\input s -> head . midEl $ runCodeWInputST code [s, input]) 0 settings

amplifierLoop :: forall m a r. (Monad m, MArray a Int m, MonadRef r m) => [a Int Int] -> [Int] -> [Int] -> m Int
amplifierLoop typeProxy code settings = do
  machines :: [Machine a r] <- sequence [createMachine code [s] | s <- settings]
  let singleRun inp0 = foldM (\input machine -> let output :: m [Int] = updateMachineInput [input] machine >>= runMachine >>= getOutputs in last <$> output) inp0 machines
  let loop inp = do
        output <- singleRun inp
        state <- readRef . mState . last $ machines
        -- print (output, state)
        if state == Halted
          then return output
          else loop output
  loop 0

amplifierLoopIO :: [Int] -> [Int] -> IO Int
amplifierLoopIO code settings = do
  machines :: [Machine (IOArray) IORef] <- sequence [createMachine code [s] | s <- settings]
  let singleRun inp0 = foldM (\input machine -> let output :: IO [Int] = updateMachineInput [input] machine >>= runMachine >>= getOutputs in last <$> output) inp0 machines
  let loop inp = do
        output <- singleRun inp
        state <- readRef . mState . last $ machines
        -- print (output, state)
        if state == Halted
          then return output
          else loop output
  loop 0
solution1 :: [Int] -> Int
solution1 code = maximum $ runST $ amplifierLoopST [] code
 where
  amplifierLoopST :: ([STArray s Int Int]) -> [Int] -> ST s [Int]
  amplifierLoopST ls code = sequence $ amplifierLoop ls code <$> permutations [0 .. 4]

solution2 :: [Int] -> Int
solution2 code = maximum $ runST $ amplifierLoopST [] code
 where
  amplifierLoopST :: ([STArray s Int Int]) -> [Int] -> ST s [Int]
  amplifierLoopST ls code = sequence $ amplifierLoop ls code <$> permutations [5 .. 9]

getSolutions7 = getSolutions codeParser solution1 solution2
