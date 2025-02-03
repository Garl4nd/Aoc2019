{-# LANGUAGE NondecreasingIndentation #-}
{-# LANGUAGE TypeApplications #-}

module N23 (getSolutions23) where

import Control.Monad (foldM, forM, unless, when, (<=<), (>=>))
import Data.Array.IO (IOArray)
import Data.IORef (IORef)
import Data.IntMap.Lazy ((!))
import qualified Data.IntMap.Lazy as M
import qualified Data.Sequence as S
import IntCode
import Useful

type QueueMap = M.IntMap [GridPos]
type IOMachine = Machine IOArray IORef

runNetwork :: Code -> Int -> IO (Int, Int)
runNetwork code part = do
  machines <- sequence [runMachine [i] =<< createMachine @IOArray code | i <- [0 .. 49]]
  runLoop machines part

runLoop :: [IOMachine] -> Int -> IO GridPos
runLoop machines part =
  let
    -- updateQueues :: QueueMap -> Int -> IO (QueueMap, Maybe GridPos, Bool)
    -- updateQueues queues natPacket machineIdx = do
    --   let machine = machines !! machineIdx
    --       currentQueue = queues ! machineIdx
    --       inputs = if null currentQueue then [-1] else concatMap (\(x, y) -> [x, y]) currentQueue
    --   outputs <- chunksOf 3 <$> (getOutputs =<< runMachine inputs machine)
    --   -- unless (null currentQueue && all (== []) outputs) $ print (machineIdx, currentQueue, inputs, outputs)
    --   let updatedQueues = foldl updateQueue (queues' natPacket)  outputs
    --       updateQueue (queue, natPacket') input = case input of
    --         [] -> (queue, natPacket)
    --         [adress, x, y] -> if adress == 255 then (queue, Just (x,y)) else (M.insertWith (++) adress [(x, y)] queue, natPacket')
    --       isIdle = all null updatedQueues &&   True
    --   return (updatedQueues, all null updatedQueues)
    -- testOutputs :: IO Bool
    -- testOutputs = do
    --   outputs <- mapM (runMachine [-1] >=> getOutputs) machines
    --   if all (== []) outputs then return True else return False

    collectOutputs :: QueueMap -> IO [Int]
    collectOutputs queueMap = fmap concat . forM (zip (M.elems queueMap) machines) $ \(currentQueue, machine) -> do
      let inputs = if null currentQueue then [-1] else concatMap (\(x, y) -> [x, y]) currentQueue
      runMachine inputs machine >>= getOutputs
    updatePackets :: (QueueMap, Maybe GridPos) -> [Int] -> (QueueMap, Maybe GridPos)
    updatePackets (queueMap', natPacket') [adress, x, y] = if adress == 255 then (queueMap', Just (x, y)) else (M.insertWith (++) adress [(x, y)] queueMap', natPacket')

    loop :: QueueMap -> Maybe GridPos -> Maybe GridPos -> IO GridPos
    loop queueMap natPacket lastSentPacket = do
      -- print ("QueueMap:", queueMap)
      outputs <- chunksOf 3 <$> collectOutputs queueMap
      let (newQueueMap, newNatPacket) = foldl updatePackets ([] <$ queueMap, natPacket) outputs
          networkIsIdle = all null queueMap && null outputs
      -- print (lastSentPacket, newNatPacket, natPacket, queueMap, newQueueMap, newNatPacket)
      if
        | part == 1, Just res <- newNatPacket -> return res
        | part == 2, networkIsIdle, Just res@(_, y) <- lastSentPacket, Just (_, y') <- newNatPacket, y == y' -> return res
        | part == 2, networkIsIdle, Just natPacket' <- newNatPacket -> loop (M.insert 0 [natPacket'] newQueueMap) newNatPacket newNatPacket
        | otherwise -> loop newQueueMap newNatPacket lastSentPacket
   in
    loop (M.fromList $ [(i, []) | i <- [0 .. 49]]) Nothing Nothing

getSolutions23 inputFile = do
  code <- codeParser <$> readFile inputFile
  sol1 <- snd <$> runNetwork code 1
  sol2 <- snd <$> runNetwork code 2
  return (sol1, sol2)
