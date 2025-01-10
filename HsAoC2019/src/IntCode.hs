{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}

module IntCode (createMachine, runMachine, runCodeWInputST, codeParser, Machine (..), MachineState (..), MachineResult (..), mState, getOutputs) where

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

import Control.Monad.Fix (fix)
import Data.Array.Base (freeze)
import Data.Array.IO (IOArray)
import Data.Foldable
import Useful (splitOn)

codeParser :: String -> [Int]
codeParser = map read . splitOn ','
data MachineState = Running | Halted | WaitingForInput deriving (Eq, Show)
data Machine a r = Machine
  { mCode :: a Int Int
  , ptrRef :: r Int
  , baseRef :: r Int
  , inputsRef :: r [Int]
  , outputsRef :: r [Int]
  , mState :: r MachineState
  }
data MachineResult = MachineResult
  { finalCode :: [Int]
  , machineOutputs :: [Int]
  , machineState :: MachineState
  }
  deriving (Show)

createMachine :: (MArray a Int m, MonadRef r m) => [Int] -> m (Machine a r)
createMachine code = do
  ar <- newArray (0, 5 * length code - 1) 0
  ptrRef <- newRef 0
  baseRef <- newRef 0
  inputsRef <- newRef []
  outputsRef <- newRef []
  mState <- newRef Running
  forM_ (zip code [0 ..]) $ \(val, pos) -> writeArray ar pos val
  return Machine{mCode = ar, ptrRef, baseRef, inputsRef, outputsRef, mState}

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

runMachine :: (MArray a Int m, MonadRef r m) => [Int] -> Machine a r -> m (Machine a r)
runMachine inputs machine@Machine{mState, inputsRef, outputsRef} = do
  currentState <- readRef mState
  if currentState == Halted
    then return machine
    else do
      writeRef mState Running
      modifyRef inputsRef (++ inputs)
      writeRef outputsRef []
      fix $ \loop -> do
        stepMachine machine
        state <- readRef mState
        when (state == Running) loop

      return machine

stepMachine :: (MArray a Int m, MonadRef r m) => Machine a r -> m ()
stepMachine Machine{mCode, ptrRef, baseRef, inputsRef, outputsRef, mState} = do
  ptr <- readRef ptrRef
  modesAndOpCode <- readArray mCode ptr
  let (modes, opCode) = parseModesAndOpCode modesAndOpCode
  let positionGetter mode pos = case mode of
        PositionMode -> return pos
        RelativeMode -> (+ pos) <$> readRef baseRef
        _ -> error "This is not allowed"
  let valGetter mode pos = case mode of
        ImmediateMode -> return pos
        _ -> positionGetter mode pos >>= readArray mCode
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
      let (mode1 : mode2 : mode3 : _) = modes
      if
        | opCode `elem` ternaryOps -> do
            inputVal1 <- valGetter mode1 =<< readArray mCode (ptr + 1)
            inputVal2 <- valGetter mode2 =<< readArray mCode (ptr + 2)
            outputPos <- positionGetter mode3 =<< readArray mCode (ptr + 3)
            writeArray mCode outputPos (op opCode inputVal1 inputVal2)
            modifyRef ptrRef (+ 4)
        | opCode `elem` binaryOps -> do
            inputVal1 <- valGetter mode1 =<< readArray mCode (ptr + 1)
            inputVal2 <- valGetter mode2 =<< readArray mCode (ptr + 2)
            let newPos = op opCode inputVal1 inputVal2
            writeRef ptrRef newPos
        | opCode `elem` unaryOps -> do
            if opCode == inpC
              then do
                targetPos <- positionGetter mode1 =<< readArray mCode (ptr + 1)
                inputLs <- readRef inputsRef
                case inputLs of
                  [] -> writeRef mState WaitingForInput
                  (currentInput : remainingInput) -> do
                    writeArray mCode targetPos currentInput
                    writeRef inputsRef remainingInput
                    modifyRef ptrRef (+ 2)
              else do
                targetVal <- valGetter mode1 =<< readArray mCode (ptr + 1)
                if opCode == outC
                  then modifyRef outputsRef (++ [targetVal])
                  else
                    when (opCode == moveBaseC) $
                      modifyRef baseRef (+ targetVal)
                modifyRef ptrRef (+ 2)
        | otherwise -> return ()

getOutputs :: (MArray a Int m, MonadRef r m) => Machine a r -> m [Int]
getOutputs Machine{outputsRef} = readRef outputsRef

getMachineResult :: forall a r m. (MArray a Int m, MonadRef r m) => Machine a r -> m MachineResult -- A.Array Int Int
getMachineResult Machine{mCode, outputsRef, mState} = do
  output <- readRef outputsRef
  ar :: A.UArray Int Int <- freeze mCode
  state <- readRef mState
  return $ MachineResult (A.elems ar) output state

runCodeWInput :: forall a r m. (MArray a Int m, MonadRef r m) => [Int] -> [Int] -> m MachineResult -- A.Array Int Int
runCodeWInput input code = do
  machine :: Machine a r <- runMachine input =<< createMachine code
  getMachineResult machine

runCodeWInputIO :: [Int] -> [Int] -> IO MachineResult -- A.Array Int Int
runCodeWInputIO = runCodeWInput @IOArray

runCodeWInputST :: [Int] -> [Int] -> MachineResult -- A.Array Int Int
runCodeWInputST input code = runST stCalc
 where
  stCalc :: forall s. ST s MachineResult
  stCalc = runCodeWInput @(STUArray s) input code
