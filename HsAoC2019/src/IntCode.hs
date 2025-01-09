{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}

module IntCode (insertCode, createMachine, runMachine, runCodeWInputST, codeParser, Machine (..), MachineState (..), MachineResult (..), mState, updateMachineInput, getOutputs) where

import Control.Monad (forM_, unless, when, (<=<), (>=>))
import Control.Monad.Ref
import Control.Monad.ST (ST, runST)
import Data.Array.Base (MArray (newArray), readArray, writeArray)
import Data.Array.ST (STUArray, runSTArray)
import qualified Data.Array.Unboxed as A
import Data.List (unfoldr)
import Data.Tuple (swap)
import Debugging (traceWInfo)

-- import GHC.Arr (freezeSTArray)

import Data.Array.Base (freeze)
import Data.Array.IO.Internals (IOArray (IOArray))
import Data.Foldable
import Useful (splitOn)

codeParser :: String -> [Int]
codeParser = map read . splitOn ','
data MachineState = Running | Halted | WaitingForInput deriving (Eq, Show)
data Machine a r = Machine
  { mCode :: a Int Int
  , ptrRef :: r Int
  , baseRef :: r Int
  , inputs :: r [Int]
  , outputs :: r [Int]
  , mState :: r MachineState
  }
data MachineResult = MachineResult
  { finalCode :: [Int]
  , machineOutputs :: [Int]
  , machineState :: MachineState
  }
  deriving (Show)
insertCode :: (MArray a Int m, MonadRef r m) => [Int] -> m (Machine a r)
insertCode = flip createMachine []

createMachine :: (MArray a Int m, MonadRef r m) => [Int] -> [Int] -> m (Machine a r)
createMachine code input = do
  ar <- newArray (0, length code - 1) 0
  ptrRef <- newRef 0
  baseRef <- newRef 0
  inputs <- newRef input
  outputs <- newRef []
  mState <- newRef Running
  forM_ (zip code [0 ..]) $ \(val, pos) -> writeArray ar pos val
  return Machine{mCode = ar, ptrRef, baseRef, inputs, outputs, mState}

addC, mulC, endC, inpC, outC, jumpTC, jumpFC, lessC, equalsC, moveBaseC :: Int
addC = 1
mulC = 2
inpC = 3
outC = 4
jumpTC = 5
jumpFC = 6
lessC = 7
equalsC = 8
moveBaseC = 9
endC = 99

unaryOps, binaryOps, ternaryOps :: [Int]
ternaryOps = [addC, mulC, lessC, equalsC]
binaryOps = [jumpTC, jumpFC]
unaryOps = [inpC, outC, moveBaseC]

data Mode = PositionMode | ImmediateMode | RelativeMode deriving (Eq, Show)

numToMode :: Int -> Mode
numToMode 0 = PositionMode
numToMode 1 = ImmediateMode
numToMode 2 = RelativeMode
numToMode _ = error "Undefined mode"

parseModesAndOpCode :: Int -> ([Mode], Int)
parseModesAndOpCode num =
  let (modeDigits, opCode) = num `quotRem` 100
      modes = unfoldr (\n -> if n == 0 then Nothing else Just $ swap $ n `quotRem` 10) modeDigits
   in (numToMode <$> modes ++ repeat 0, opCode)

updateMachineInput :: (MArray a Int m, MonadRef r m) => [Int] -> Machine a r -> m (Machine a r)
updateMachineInput input machine@Machine{inputs} = do
  inputLs <- readRef inputs
  writeRef inputs (inputLs ++ input)
  return machine{inputs = inputs}

runMachine :: (MArray a Int m, MonadRef r m) => Machine a r -> m (Machine a r)
runMachine machine@Machine{mState} = do
  writeRef mState Running
  let loop = do
        stepMachine machine
        state <- readRef mState
        when (state == Running) loop
  loop
  return machine

stepMachine :: (MArray a Int m, MonadRef r m) => Machine a r -> m ()
stepMachine Machine{mCode, ptrRef, baseRef, inputs, outputs, mState} = do
  ptr <- readRef ptrRef
  modesAndOpCode <- readArray mCode ptr
  let (modes, opCode) = parseModesAndOpCode modesAndOpCode
  let valGetter mode pos = case mode of
        ImmediateMode -> return pos
        RelativeMode -> readRef baseRef >>= \base -> readArray mCode (base + pos)
        _ -> readArray mCode pos
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
            modifyRef ptrRef (+ 4)
        | opCode `elem` binaryOps -> do
            inputPos1 <- readArray mCode (ptr + 1)
            inputPos2 <- readArray mCode (ptr + 2)
            inputVal1 <- valGetter mode1 inputPos1
            inputVal2 <- valGetter mode2 inputPos2
            let newPos = op opCode inputVal1 inputVal2
            writeRef ptrRef newPos
        | opCode `elem` unaryOps -> do
            targetPos <- readArray mCode (ptr + 1)
            if opCode == inpC
              then do
                inputLs <- readRef inputs
                case inputLs of
                  [] -> writeRef mState WaitingForInput
                  (currentInput : remainingInput) -> do
                    outputPos <- case mode1 of
                      PositionMode -> return targetPos
                      RelativeMode -> (+ targetPos) <$> readRef baseRef
                    writeArray mCode outputPos currentInput
                    -- writeArray mCode outPos currentInput
                    writeRef inputs remainingInput
                    modifyRef ptrRef (+ 2)
              else
                if opCode == outC
                  then do
                    outputLs <- readRef outputs
                    outputVal <- valGetter mode1 targetPos
                    writeRef outputs $ outputLs ++ [outputVal]
                    modifyRef ptrRef (+ 2)
                  else when (opCode == moveBaseC) $ do
                    baseJumpVal <- valGetter mode1 targetPos
                    modifyRef baseRef (+ baseJumpVal)
                    modifyRef ptrRef (+ 2)
        | otherwise -> return ()

getOutputs :: (MArray a Int m, MonadRef r m) => Machine a r -> m [Int]
getOutputs Machine{outputs} = readRef outputs

runCodeWInput :: forall a r m. (MArray a Int m, MonadRef r m) => [Int] -> [Int] -> m MachineResult -- A.Array Int Int
runCodeWInput code input = do
  Machine{mCode, outputs, mState} :: Machine a r <- runMachine =<< createMachine extCode input
  output <- readRef outputs
  ar :: A.UArray Int Int <- freeze mCode
  state <- readRef mState
  return $ MachineResult (A.elems ar) output state
 where
  extCode = code ++ replicate (10 * length code) 0

runCodeWInputIO :: [Int] -> [Int] -> IO MachineResult -- A.Array Int Int
runCodeWInputIO code input = runCodeWInput @IOArray code input where

runCodeWInputST :: [Int] -> [Int] -> MachineResult -- A.Array Int Int
runCodeWInputST code input = runST stCalc
 where
  stCalc :: forall s. ST s MachineResult
  stCalc = runCodeWInput @(STUArray s) code input
