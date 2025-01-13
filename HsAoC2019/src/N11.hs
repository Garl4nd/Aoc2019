{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}

module N11 () where

import Control.Monad (unless, when)
import Control.Monad.Fix (fix)
import Control.Monad.ST
import Data.Array.Base
import Data.Array.ST (runSTUArray)
import qualified Data.Array.Unboxed as A
import IntCode
import Useful

data Direction = U | D | L | R deriving (Show, Eq)
width, height :: Int
width = 100
height = 100

paintShip :: [Int] -> A.UArray GridPos Bool
paintShip code = runSTUArray calc
 where
  calc :: forall s. ST s (STUArray s GridPos Bool)
  calc = do
    shipAr <- newArray @(STUArray s) ((-height, -width), (height, width)) 0
    paintedPanels <- newArray ((-height, -width), (height, width)) False
    computer <- createMachine @(STUArray s) code
    flip fix ((0, 0), U) $ \loop (currentPos, currentDir) -> do
      currentColor <- readArray shipAr currentPos
      MachineResult{machineState, machineOutputs} <- getMachineResult =<< runMachine [currentColor] computer
      unless (machineState == Halted) $ do
        let [newColor, turnCode] = machineOutputs
        writeArray shipAr currentPos newColor
        when (newColor == 1) $ writeArray paintedPanels currentPos True
        let newDir = newDirection turnCode currentDir
        let newPos = turn currentPos newDir
        loop (newPos, newDir)

    return paintedPanels

newDirection :: Int -> Direction -> Direction
newDirection 0 = \case
  U -> L
  D -> R
  L -> D
  R -> U
newDirection _ = \case
  U -> R
  D -> L
  L -> U
  R -> D

turn :: GridPos -> Direction -> GridPos
turn (y, x) U = (y - 1, x)
turn (y, x) D = (y + 1, x)
turn (y, x) L = (y, x - 1)
turn (y, x) R = (y, x + 1)
