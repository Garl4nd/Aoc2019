{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}

module N7 (getSolutions7) where

import Control.Monad (foldM)
import Control.Monad.ST
import Data.List (permutations)
import IntCode

import Control.Monad.Ref
import Data.Array.Base (MArray)
import Useful (getSolutions)
import Data.Array.ST (STUArray)
import Control.Monad.Fix (fix)

amplifierLoop :: forall a m r. (MArray a Int m, MonadRef r m) => [Int] -> [Int] -> m Int
amplifierLoop code settings = do
  machines :: [Machine a r] <- sequence [createMachine code [s] | s<- settings] 
  let singleRun inp0 = foldM (\input machine -> let outputs = updateMachineInput [input] machine >>= runMachine >>= getOutputs in last <$> outputs) inp0 machines
  flip fix 0 $ \loop inp -> do
        output <- singleRun inp
        state <- readRef . mState . last $ machines
        if state == Halted
          then return output
          else loop output

solution1 :: [Int] -> Int
solution1 code = maxAmplifierOutput code [0 .. 4]

solution2 :: [Int] -> Int
solution2 code = maxAmplifierOutput code [5 .. 9]

maxAmplifierOutput :: [Int] -> [Int] -> Int
maxAmplifierOutput code settingRange = maximum $ runST resultsST
 where
  resultsST = mapM (amplifierLoopST code) $ permutations settingRange
  amplifierLoopST :: forall s. [Int] -> [Int] -> ST s Int
  amplifierLoopST = amplifierLoop @(STUArray s)

getSolutions7 = getSolutions codeParser solution1 solution2
