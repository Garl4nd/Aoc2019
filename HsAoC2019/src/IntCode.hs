{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}

module IntCode (insertCode, runCode, runCodeST)
where

import Control.Monad (forM_, unless)
import Control.Monad.Ref
import Control.Monad.ST (ST, runST)
import qualified Data.Array as A
import Data.Array.Base (MArray (newArray), readArray, writeArray)
import Data.Array.ST (runSTArray)

data Machine a r = Machine
  { mCode :: a Int Int
  , ptrRef :: r Int
  }

insertCode :: (MArray a Int m, MonadRef r m) => [Int] -> m (Machine a r)
insertCode code = do
  ar <- newArray (0, length code - 1) 0
  ptrRef <- newRef 0
  forM_ (zip code [0 ..]) $ \(val, pos) -> writeArray ar pos val
  return Machine{mCode = ar, ptrRef}

addC, mulC, endC :: Int
addC = 1
mulC = 2
endC = 99

runMachine :: (MArray a Int m, MonadRef r m) => m (Machine a r) -> m (Machine a r)
runMachine machineCalc = do
  machine <- machineCalc
  execMachine machine
  return machine
 where
  execMachine Machine{mCode, ptrRef} = loop
   where
    loop = do
      ptr <- readRef ptrRef
      cVal <- readArray mCode ptr
      unless (cVal == endC) $ do
        inputPos1 <- readArray mCode (ptr + 1)
        inputPos2 <- readArray mCode (ptr + 2)
        outputPos <- readArray mCode (ptr + 3)
        inputVal1 <- readArray mCode inputPos1
        inputVal2 <- readArray mCode inputPos2
        let
          op n
            | n == addC = (+)
            | n == mulC = (*)
            | otherwise = error "Undefined operator"
          jump = 4
        writeArray mCode outputPos (op cVal inputVal1 inputVal2)
        writeRef ptrRef (ptr + jump)
        loop

runCode :: (MArray a Int m, MonadRef r m) => [Int] -> m (a Int Int)
runCode = fmap mCode . runMachine . insertCode
runCodeST :: [Int] -> [Int] -- A.Array Int Int
runCodeST code = A.elems $ runSTArray $ runCode code
