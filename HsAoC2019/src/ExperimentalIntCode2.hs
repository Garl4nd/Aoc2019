{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeFamilies #-}

module ExperimentalIntCode2 (insertCode)
where

import Control.Monad (forM_, unless)
import Control.Monad.Ref
import Control.Monad.ST (ST, runST)
import qualified Data.Array as A
import Data.Array.Base (MArray (newArray), readArray, writeArray)
import Data.Array.IO.Internals (IOArray (IOArray))
import Data.Array.ST (runSTArray)
import GHC.Arr (STArray (STArray))
import GHC.IORef (IORef (IORef))
import Data.STRef (STRef)

data family Ar :: (* -> *) -> * -> * -> *
data instance Ar IO i e = IOArray i e
data instance Ar (ST s) i e = STArray s i e
data family Ref :: (* -> *) -> * -> *
data instance Ref IO e = IORef e
data instance Ref (ST s) e = STRef e

data Machine m = Machine
  { ptrRef :: Ref m Int
  , mCode :: Ar m Int Int
  }

insertCode :: (MArray (Ar m) Int m, MonadRef (Ref m) m) => [Int] -> m (Machine m)
insertCode code = do
  ar <- newArray (0, length code - 1) 0
  ptrRef <- newRef 0
  forM_ (zip code [0 ..]) $ \(val, pos) -> writeArray ar pos val
  return Machine{mCode = ar, ptrRef}

addC, mulC, endC :: Int
addC = 1
mulC = 2
endC = 99

runMachine :: (MArray (Ar m) Int m, MonadRef (Ref m) m) => m (Machine m) -> m (Machine m)
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

-- runCode :: (MArray (Ar m) Int m, MonadRef (Ref m) m) => [Int] -> m ((Ar m) Int Int)
-- runCode = fmap mCode . runMachine . insertCode
-- runCodeST :: [Int] -> [Int] -- A.Array Int Int
-- runCodeST code = let
--      machine :: ST s (Machine (ST s))
--      machine = insertCode code 
--      stAr :: ST s (STArray s Int Int ) 
--      stAr = do 
--        Machine {mCode, ptrRef} <- runMachine machine 
--        return mCode 
--     in []
