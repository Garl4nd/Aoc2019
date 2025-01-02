{-# LANGUAGE FlexibleContexts #-}

module IntCode (insertCode, runCode, runCodeST)
where

import Control.Monad (forM_, unless)
import Control.Monad.ST (ST, runST)
import qualified Data.Array as A
import Data.Array.Base (MArray (newArray), readArray, writeArray)
import Data.Array.ST (STArray, runSTArray)

data (Monad m, MArray a Int m) => Machine a m = Machine
  { mCode :: a Int Int
  , ptr :: Int
  }

insertCode :: (MArray a Int m) => [Int] -> m (Machine a m)
insertCode code = do
  ar <- newArray (0, length code - 1) 0
  forM_ (zip code [0 ..]) $ \(val, pos) -> writeArray ar pos val
  return Machine{mCode = ar, ptr = 0}

addC, mulC, endC :: Int
addC = 1
mulC = 2
endC = 99

runMachine :: (MArray a Int m) => m (Machine a m) -> m (Machine a m)
runMachine mMachine = do
  machine@Machine{mCode = code, ptr = cPtr} <- mMachine
  cVal <- readArray code cPtr
  if cVal == endC
    then mMachine
    else do
      inputPos1 <- readArray code (cPtr + 1)
      inputPos2 <- readArray code (cPtr + 2)
      outputPos <- readArray code (cPtr + 3)
      inputVal1 <- readArray code inputPos1
      inputVal2 <- readArray code inputPos2
      let
        op n
          | n == 1 = (+)
          | n == 2 = (*)
          | otherwise = error "Undefined operator"
        jump = 4
      writeArray code outputPos (op cVal inputVal1 inputVal2) >> runMachine (return machine{ptr = cPtr + jump})

runCode :: (MArray a Int m) => [Int] -> m (a Int Int)
runCode = fmap mCode . runMachine . insertCode
runCodeST :: [Int] -> [Int] -- A.Array Int Int
runCodeST code = A.elems $ runSTArray $ runCode code
