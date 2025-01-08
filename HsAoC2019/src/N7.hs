{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}

module N7 (getSolutions7) where

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

amplifierLoop :: forall a m r. (MArray a Int m, MonadRef r m) => [Int] -> [Int] -> m Int
amplifierLoop code settings = do
  machines :: [Machine a r] <- sequence [createMachine code [s] | s <- settings]
  let singleRun inp0 = foldM (\input machine -> let output :: m [Int] = updateMachineInput [input] machine >>= runMachine >>= getOutputs in last <$> output) inp0 machines
  let loop inp = do
        output <- singleRun inp
        state <- readRef . mState . last $ machines
        if state == Halted
          then return output
          else loop output
  loop 0

solution1 :: [Int] -> Int
solution1 code = amplifierOutput code [0 .. 4]

solution2 :: [Int] -> Int
solution2 code = amplifierOutput code [5 .. 9]

amplifierOutput :: [Int] -> [Int] -> Int
amplifierOutput code settingRange = maximum $ runST resultsST
 where
  resultsST = mapM (amplifierLoopST code) $ permutations settingRange
  amplifierLoopST :: forall s. [Int] -> [Int] -> ST s Int
  amplifierLoopST = amplifierLoop @(STArray s)

getSolutions7 = getSolutions codeParser solution1 solution2
