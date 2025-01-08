{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}

module IntCode (insertCode, createMachine, runCode, runMachine, runCodeST, runCodeWInputST, codeParser, Machine (..), MachineState (..), mState, updateMachineInput, getOutputs) where

import Control.Monad (forM_, unless, when, (<=<), (>=>))
import Control.Monad.Ref
import Control.Monad.ST (runST)
import qualified Data.Array as A
import Data.Array.Base (MArray (newArray), readArray, writeArray)
import Data.Array.ST (runSTArray)
import Data.List (unfoldr)
import Data.Tuple (swap)
import Debugging (traceWInfo)
import GHC.Arr (freezeSTArray)
import Useful (splitOn)

codeParser :: String -> [Int]
codeParser = map read . splitOn ','
data MachineState = Running | Halted | WaitingForInput deriving (Eq, Show)
data Machine a r = Machine
  { mCode :: a Int Int
  , ptrRef :: r Int
  , inputs :: r [Int]
  , outputs :: r [Int]
  , mState :: r MachineState
  }

insertCode :: (MArray a Int m, MonadRef r m) => [Int] -> m (Machine a r)
insertCode = flip createMachine []

createMachine :: (MArray a Int m, MonadRef r m) => [Int] -> [Int] -> m (Machine a r)
createMachine code input = do
  ar <- newArray (0, length code - 1) 0
  ptrRef <- newRef 0
  inputs <- newRef input
  outputs <- newRef []
  mState <- newRef Running
  forM_ (zip code [0 ..]) $ \(val, pos) -> writeArray ar pos val
  return Machine{mCode = ar, ptrRef, inputs, outputs, mState}

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

updateMachineInput :: (MArray a Int m, MonadRef r m) => [Int] -> Machine a r -> m (Machine a r)
updateMachineInput input machine@Machine{inputs} = do
  inputLs <- readRef inputs
  writeRef inputs (inputLs ++ input)
  return machine{inputs = inputs}

runMachine :: (MArray a Int m, MonadRef r m) => Machine a r -> m (Machine a r)
runMachine machine@Machine{mState} = do
  writeRef mState Running
  execMachine machine
  return machine
 where
  execMachine Machine{mCode, ptrRef, inputs, outputs, mState} = loop
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
      if opCode == endC
        then writeRef mState Halted
        else do
          let (mode1 : mode2 : _) = modes
          if
            | opCode `elem` ternaryOps -> do
                inputPos1 <- readArray mCode (ptr + 1)
                inputPos2 <- readArray mCode (ptr + 2)
                outputPos <- readArray mCode (ptr + 3)
                inputVal1 <- valGetter mode1 inputPos1
                inputVal2 <- valGetter mode2 inputPos2
                writeArray mCode outputPos (op opCode inputVal1 inputVal2)
                writeRef ptrRef (ptr + 4)
            | opCode `elem` binaryOps -> do
                inputPos1 <- readArray mCode (ptr + 1)
                inputPos2 <- readArray mCode (ptr + 2)
                inputVal1 <- valGetter mode1 inputPos1
                inputVal2 <- valGetter mode2 inputPos2
                let newPos = op opCode inputVal1 inputVal2
                writeRef ptrRef newPos
            | opCode `elem` unaryOps -> do
                targetPos <- readArray mCode (ptr + 1)
                when (opCode == inpC) $ do
                  inputLs <- readRef inputs
                  case inputLs of
                    [] -> writeRef mState WaitingForInput
                    (currentInput : remainingInput) -> do
                      writeArray mCode targetPos currentInput
                      writeRef inputs remainingInput
                      writeRef ptrRef (ptr + 2)
                when (opCode == outC) $ do
                  outputLs <- readRef outputs
                  outputVal <- valGetter mode1 targetPos
                  writeRef outputs $ outputLs ++ [outputVal]
                  writeRef ptrRef (ptr + 2)
            | otherwise -> return ()
          state <- readRef mState
          when (state == Running) loop

getOutputs :: (MArray a Int m, MonadRef r m) => Machine a r -> m [Int]
getOutputs Machine{outputs} = readRef outputs

runCodeWInput :: (MArray a Int m, MonadRef r m) => [Int] -> [Int] -> m (a Int Int)
runCodeWInput = ((fmap mCode . runMachine) <=<) . createMachine

runCode :: (MArray a Int m, MonadRef r m) => [Int] -> m (a Int Int)
runCode = flip runCodeWInput []

runCodeST :: [Int] -> [Int] -- A.Array Int Int
runCodeST code = A.elems $ runSTArray $ runCode code

runCodeWInputST :: [Int] -> [Int] -> ([Int], [Int], MachineState) -- A.Array Int Int
runCodeWInputST code input =
  let (ar, output, state) = runST $ do
        Machine{mCode, outputs, mState} <- runMachine =<< createMachine code input
        output <- readRef outputs
        ar <- freezeSTArray mCode
        state <- readRef mState
        return (ar, output, state)
   in (A.elems ar, output, state)
