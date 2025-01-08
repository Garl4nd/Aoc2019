{-# LANGUAGE FlexibleContexts #-}

module N7 () where

import Control.Monad (foldM)
import Control.Monad.Ref (readRef)
import Control.Monad.ST
import Data.Array.IO (IOArray)
import Data.Array.ST (STArray)
import Data.IORef (IORef)
import Data.List (foldl')
import Data.STRef (STRef)
import Debugging (traceWInfo)
import IntCode

midEl (_, x, _) = x

runAmplifiers :: [Int] -> [Int] -> Int
runAmplifiers code settings = foldl' (\input s -> head . midEl $ runCodeWInputST code [s, input]) 0 settings

amplifierLoop :: [Int] -> [Int] -> Int
amplifierLoop code settings = runST $ do
  machines :: [Machine (STArray s) (STRef s)] <- sequence [createMachine code [s] | s <- settings]
  let singleRun mchns inp0 = foldM (\input (s, machine) -> let output = updateMachineInput [s, input] machine >>= runMachine >>= getOutputs in head <$> output) inp0 $ zip settings mchns
  output0 <- singleRun machines 0
  output1 <- singleRun machines $ traceWInfo True "o0" $ output0
  return output1

amplifierLoopIO :: [Int] -> [Int] -> IO Int
amplifierLoopIO code settings = do
  machines :: [Machine (IOArray) IORef] <- sequence [createMachine code [s] | s <- settings]
  let singleRun inp0 = foldM (\input machine -> let output = updateMachineInput [input] machine >>= runMachine >>= getOutputs in head <$> output) inp0 machines
  mStates <- sequence [readRef $ mState machine | machine <- machines]
  print mStates
  output0 <- singleRun 0
  mStates <- sequence [readRef $ mState machine | machine <- machines]
  print mStates
  output1 <- singleRun $ traceWInfo True "o0" $ output0
  print output1
  return output1
