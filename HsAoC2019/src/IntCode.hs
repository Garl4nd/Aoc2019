{-# LANGUAGE FlexibleContexts #-}

module IntCode ()
where

import Control.Monad (forM_)
import Control.Monad.ST (ST, runST)
import Data.Array.Base (MArray (newArray), writeArray)
import Data.Array.IO (IOArray)
import Data.Array.ST (STArray)
import GHC.TopHandler (runIO)

data (Monad m, MArray a Int m) => Machine a m = Machine
  { mCode :: a Int Int
  , ptr :: Int
  }
  deriving (Show)

insertCode :: (Monad m, MArray a Int m) => [Int] -> m (Machine a m)
insertCode code = do
  ar <- newArray (0, length code - 1) 0
  forM_ (zip code [0 ..]) $ \(val, pos) -> writeArray ar pos val
  return Machine{mCode = ar, ptr = 0}

runInsert :: [Int] -> IO (Machine IOArray IO)
runInsert code = runIO $ insertCode code
