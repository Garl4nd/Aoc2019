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
import Data.List (intercalate)

data Direction = U | D | L | R deriving (Show, Eq)
width, height :: Int
width =  45
height = 45

paintShip :: Int -> [Int] -> (A.Array GridPos Int, A.UArray GridPos Bool)
paintShip initTile code = runST calc
 where
  calc :: forall s. ST s (A.Array GridPos Int, A.UArray GridPos Bool)
  calc = do
    shipAr <- newArray @(STUArray s) ((-height, -width), (height, width)) 0
    writeArray shipAr (0,0) initTile 
    paintedPanels <- newArray @(STUArray s) ((-height, -width), (height, width)) False
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
    shipArFr <- freeze shipAr 
    paintedPanelsFr <- freeze paintedPanels 
    return (shipArFr, paintedPanelsFr)

solution1 :: [Int] -> Int 
solution1 code = let 
  (_, paintedPanels) = paintShip 0 code 
  in count True $ A.elems paintedPanels

solution2 :: [Int] -> String 
solution2 code = let 
  (finalPaintEncoded, _) = paintShip 1 code 
  finalPaint = (\x -> if x>0 then 'O' else ' ') <$> finalPaintEncoded
  in intercalate "\n" $  charGridToStr finalPaint


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
