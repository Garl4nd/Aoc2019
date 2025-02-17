{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}

module IntCode (createMachine, runMachine, runCodeWInputST, getMachineResult, codeParser, Machine (..), MachineState (..), MachineResult (..), mState, getOutputs, runProgramIO, talkToMachine, Code) where

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
import Data.Char (chr, ord)
import Data.Foldable
import GHC.IOArray
import GHC.IORef
import Useful (splitOn)

type Code = [Int]

codeParser :: String -> Code
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
  { finalCode :: Code
  , machineOutputs :: [Int]
  , machineState :: MachineState
  }
  deriving (Show)

data UnaryInst = Input | Output | MoveBase
data BinaryInst = JumpTrue | JumpFalse 
data TernaryInst = Add | Mul | Less | Equals 
data Inst = Unary UnaryInst | Binary BinaryInst | Ternary TernaryInst  
-- unaryOps, binaryOps, ternaryOps :: [Int]
-- ternaryOps = [addC, mulC, lessC, equalsC]
-- binaryOps = [jumpTC, jumpFC]
-- unaryOps = [inpC, outC, moveBaseC]


createMachine :: (MArray a Int m, MonadRef r m) => Code -> m (Machine a r)
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

opcodeToOp :: Int -> Inst
opcodeToOp 1 = Ternary Add 
opcodeToOp 2 = Ternary Mul
opcodeToOp 3 = Unary Input
opcodeToOp 4 = Unary Output 
opcodeToOp 5 = Binary JumpTrue  
opcodeToOp 6 = Binary JumpFalse  
opcodeToOp 7 = Ternary Less  
opcodeToOp 8 = Ternary Equals 
opcodeToOp 9 = Unary MoveBase  
opcodeToOp _ = undefined "Wrong code"

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

parseModesAndOpCode :: Int -> ((Mode, Mode, Mode), Int)
parseModesAndOpCode num =
  let (modeDigits, opCode) = num `quotRem` 100
      modes = unfoldr (\n -> if n == 0 then Nothing else Just $ swap $ n `quotRem` 10) modeDigits
   in (let m1:m2:m3:_ = numToMode <$> modes ++ repeat 0 in (m1,m2,m3), opCode)

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

funcForTernary :: TernaryInst -> (Int -> Int -> Int ) 
funcForTernary Add = (+)
funcForTernary Mul = (*)
funcForTernary Less =   \p1 p2 -> if p1 < p2 then 1 else 0
funcForTernary Equals = \p1 p2 -> if p1 == p2 then 1 else 0

funcForBinary :: BinaryInst -> (Int -> Int -> Int -> Int )      
funcForBinary JumpTrue p1 p2 ptr = if p1 == 0 then ptr + 3 else p2
funcForBinary JumpFalse p1 p2 ptr = if p1 /= 0 then ptr+3 else p2


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
  if opCode == endC
    then writeRef mState Halted
    else do
      let (mode1, mode2, mode3) = modes
      case opcodeToOp opCode of 
        Ternary op -> do
             inputVal1 <- valGetter mode1 =<< readArray mCode (ptr + 1)
             inputVal2 <- valGetter mode2 =<< readArray mCode (ptr + 2)
             outputPos <- positionGetter mode3 =<< readArray mCode (ptr + 3)
             writeArray mCode outputPos (funcForTernary op inputVal1 inputVal2)
             modifyRef ptrRef (+ 4)
        Binary op -> do
            inputVal1 <- valGetter mode1 =<< readArray mCode (ptr + 1)
            inputVal2 <- valGetter mode2 =<< readArray mCode (ptr + 2)
            let newPos = funcForBinary op inputVal1 inputVal2 ptr
            writeRef ptrRef newPos
        Unary Input ->  
                do
                targetPos <- positionGetter mode1 =<< readArray mCode (ptr + 1)
                inputLs <- readRef inputsRef
                case inputLs of
                  [] -> writeRef mState WaitingForInput
                  (currentInput : remainingInput) -> do
                    writeArray mCode targetPos currentInput
                    writeRef inputsRef remainingInput
                    modifyRef ptrRef (+ 2)
        Unary op ->  do
                targetVal <- valGetter mode1 =<< readArray mCode (ptr + 1)
                case op of
                  Output ->  modifyRef outputsRef (++ [targetVal])
                  MoveBase -> modifyRef baseRef (+ targetVal)
                modifyRef ptrRef (+ 2)

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

runProgramIO :: Code -> IO ()
runProgramIO code = do
  machine <- createMachine @IOArray code
  flip fix [] $ \loop input -> do
    output <- getOutputs =<< runMachine input machine
    putStrLn $ "output = " <> show output
    state <- machineState <$> getMachineResult machine
    unless (state == Halted) $ do
      print "Write your input:"
      strInput <- getLine
      let newInput = [read strInput]
      loop newInput

talkToMachine :: Code -> String -> IO ()
talkToMachine code initInput = do
  machine <- createMachine @IOArray code
  flip fix (ord <$> initInput) $ \loop input -> do
    intOutput <- getOutputs =<< runMachine input machine
    let strOutput = concatMap (\i -> if i < 256 then chr i : "" else show i) intOutput
    putStrLn strOutput
    state <- machineState <$> getMachineResult machine
    unless (state == Halted) $ do
      strInput <- getLine
      let newInput = ord <$> strInput <> "\n"
      loop newInput
