{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}

module IntCode (insertCode, runCode, runCodeST, codeParser) where

import Control.Monad (forM_, unless, when)
import Control.Monad.Ref
import Control.Monad.ST (runST)
import qualified Data.Array as A
import Data.Array.Base (MArray (newArray), readArray, writeArray)
import Data.Array.ST (runSTArray)
import Data.List (unfoldr)
import Data.Tuple (swap)
import GHC.Arr (freezeSTArray)
import Useful (splitOn)

codeParser :: String -> [Int]
codeParser = map read . splitOn ','

data Machine a r = Machine
  { mCode :: a Int Int
  , ptrRef :: r Int
  , inputs :: r [Int]
  , outputs :: r [Int]
  }

insertCode :: (MArray a Int m, MonadRef r m) => [Int] -> m (Machine a r)
insertCode code = do
  ar <- newArray (0, length code - 1) 0
  ptrRef <- newRef 0
  inputs <- newRef []
  outputs <- newRef []
  forM_ (zip code [0 ..]) $ \(val, pos) -> writeArray ar pos val
  return Machine{mCode = ar, ptrRef, inputs, outputs}

insertCodeAndInput :: (MArray a Int m, MonadRef r m) => [Int] -> [Int] -> m (Machine a r)
insertCodeAndInput code input = do
  ar <- newArray (0, length code - 1) 0
  ptrRef <- newRef 0
  inputs <- newRef input
  outputs <- newRef []
  forM_ (zip code [0 ..]) $ \(val, pos) -> writeArray ar pos val
  return Machine{mCode = ar, ptrRef, inputs, outputs}

addC, mulC, endC, inpC, outC, jumpTC, jumpFC, lessC, equalsC :: Int
addC = 1
mulC = 2
inpC = 3
outC = 4
jumpTC = 5
jumpFC = 6
lessC = 7
equalsC = 8
endC = 99
unaryOps, binaryOps, ternaryOps :: [Int]
ternaryOps = [addC, mulC, lessC, equalsC]
binaryOps = [jumpTC, jumpFC]
unaryOps = [inpC, outC]

data Mode = PositionMode | ImmediateMode deriving (Eq, Show)

numToMode :: Int -> Mode
numToMode 0 = PositionMode
numToMode 1 = ImmediateMode
numToMode _ = error "Undefined mode"

parseModesAndOpCode :: Int -> ([Mode], Int)
parseModesAndOpCode num =
  let (modeDigit, opCode) = num `quotRem` 100
      modes = unfoldr (\n -> if n == 0 then Nothing else Just $ swap $ n `quotRem` 10) modeDigit
   in (numToMode <$> modes ++ repeat 0, opCode)

runMachine :: (MArray a Int m, MonadRef r m) => m (Machine a r) -> m (Machine a r)
runMachine machineCalc = do
  machine <- machineCalc
  execMachine machine
  return machine
 where
  execMachine Machine{mCode, ptrRef, inputs, outputs} = loop
   where
    loop = do
      ptr <- readRef ptrRef
      modesAndOpCode <- readArray mCode ptr
      let (modes, opCode) = parseModesAndOpCode modesAndOpCode
      let valGetter mode pos = if mode == ImmediateMode then return pos else readArray mCode pos
      let op n
            | n == addC = (+)
            | n == mulC = (*)
            | n == lessC = (\p1 p2 -> if p1 < p2 then 1 else 0)
            | n == equalsC = (\p1 p2 -> if p1 == p2 then 1 else 0)
            | n == jumpTC = (\p1 p2 -> if p1 /= 0 then p2 else ptr + 3)
            | n == jumpFC = (\p1 p2 -> if p1 == 0 then p2 else ptr + 3)
            | otherwise = error "Undefined operator"
      unless (opCode == endC) $ do
        let (mode1 : mode2 : _) = modes
        when (opCode `elem` ternaryOps) $ do
          inputPos1 <- readArray mCode (ptr + 1)
          inputPos2 <- readArray mCode (ptr + 2)
          outputPos <- readArray mCode (ptr + 3)
          inputVal1 <- valGetter mode1 inputPos1
          inputVal2 <- valGetter mode2 inputPos2
          writeArray mCode outputPos (op opCode inputVal1 inputVal2)
          writeRef ptrRef (ptr + 4)
        when (opCode `elem` binaryOps) $ do
          inputPos1 <- readArray mCode (ptr + 1)
          inputPos2 <- readArray mCode (ptr + 2)
          inputVal1 <- valGetter mode1 inputPos1
          inputVal2 <- valGetter mode2 inputPos2
          let newPos = op opCode inputVal1 inputVal2
          writeRef ptrRef newPos
        when (opCode `elem` unaryOps) $ do
          targetPos <- readArray mCode (ptr + 1)
          when (opCode == inpC) $ do
            inputLs <- readRef inputs
            writeArray mCode targetPos (head inputLs)
          when (opCode == outC) $ do
            outputLs <- readRef outputs
            outputVal <- valGetter mode1 targetPos
            writeRef outputs $ outputLs ++ [outputVal]
          writeRef ptrRef (ptr + 2)
        loop

runCode :: (MArray a Int m, MonadRef r m) => [Int] -> m (a Int Int)
runCode = fmap mCode . runMachine . insertCode
runCodeWInput :: (MArray a Int m, MonadRef r m) => [Int] -> [Int] -> m (a Int Int)
runCodeWInput = ((fmap mCode . runMachine) .) . insertCodeAndInput

runCodeST :: [Int] -> [Int] -- A.Array Int Int
runCodeST code = A.elems $ runSTArray $ runCode code

runCodeWInputST :: [Int] -> [Int] -> ([Int], [Int]) -- A.Array Int Int
runCodeWInputST code input =
  let (ar, output) = runST $ do
        Machine{mCode, outputs} <- runMachine $ insertCodeAndInput code input
        output <- readRef outputs
        ar <- freezeSTArray mCode
        return (ar, output)
   in (A.elems ar, output)
