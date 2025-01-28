{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}

-- to solve Part 2: Split into 4 parts and modify the edge function for each part so as to ignore doors that are not present in that part. The result is the sum for each part
module N18 () where

import Control.Monad
import Data.Array ((!))
import qualified Data.Array as A
import Data.Array.ST
import Data.Bits
import Data.Char
import Data.List (find)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import qualified Data.Set as S
import GraphUtils
import Useful

-- runDijkstraST :: forall graph node. (Show node, LabeledGraph graph node) => graph -> node -> [node] -> DijkstraState node
type Key = Char
type Encoding = Int
type AugPos = (GridPos, Encoding)
type AugGraph = ArrayGraph AugPos
type LazySetGraph = LazyGraph CharGrid AugPos

grid :: A.Array GridPos Int
grid = A.array ((0, 0), (10, 10)) [((y, x), 0) | y <- [0 .. 9], x <- [0 .. 9]]
makeLazyGraphOrd :: A.Array GridPos e -> LazyGraph (A.Array GridPos e) GridPos
makeLazyGraphOrd grid = LazyGraph{nodes = grid, edgeFunc = getAugEdges, bounds = A.bounds grid}
 where
  getAugEdges pos = [(n, Dist 1) | n <- neighbors4 pos]

makeLazyGraph :: CharGrid -> [Char] -> LazySetGraph
makeLazyGraph grid doors = LazyGraph{nodes = grid, edgeFunc = getAugEdges, bounds = ((minBound, 0), (maxBound, shiftL 1 26))}
 where
  getAugEdges (pos, keys) =
    let
      walkableNeighbors = [n | n <- neighbors4 pos, inBounds n && grid ! n /= '#']
     in
      [((n, newKeys), newVal) | n <- walkableNeighbors, let (newKeys, newVal) = newKeysVal n]
   where
    newKeysVal n
      | grid ! n == '.' = (keys, Dist 1)
      | let val = grid ! n, isAsciiLower val = (keys .|. encode val, Dist 1)
      | let val = grid ! n, val `elem` doors = (keys, if keys .&. (encode $ toLower val) == 0 then Inf else Dist 1)
      | grid ! n == '@' = (keys, Dist 1)
      | otherwise = undefined
  inBounds = A.inRange bounds
  bounds@(minBound, maxBound) = A.bounds grid

encode c = shiftL 1 (ord c - ord 'a')

makeGraph :: CharGrid -> AugGraph -- LazyGraph (M.Map node) node
makeGraph grid = runSTArray $ do
  ar <- newArray ((minPos, 0), (maxPos, 2 ^ 26)) []
  forM_ passableBlocks $ \block -> do
    let neighbors = neighbors4 block

    return ()
  return ar
 where
  (minPos, maxPos) = A.bounds grid
  passableBlocks = [pos | pos <- A.indices grid, grid ! pos /= '#']

solution1 :: CharGrid -> Distance -- M.Map AugPos Distance --  DijkstraState AugPos (M.Map AugPos Distance)
solution1 charGrid =
  let
    Just start = find (\pos -> charGrid ! pos == '@') $ A.indices charGrid
    keyPos = [pos | pos <- A.indices charGrid, isAsciiLower (charGrid ! pos)]
    keyChars = [val | val <- A.elems charGrid, isAsciiLower val]
    doorChars = [val | val <- A.elems charGrid, isAsciiUpper val]
    maxVal :: Int = shiftL (encode (maximum keyChars)) 1 - 1
    targets = [(key, maxVal) | key <- keyPos]
    distMap = distanceMap $ runDijkstra @(M.Map AugPos Distance) (makeLazyGraph charGrid doorChars) (start, 0) targets
   in
    minimum $ [val | pos <- targets, let val = fromMaybe Inf $ M.lookup pos distMap]

modGrids :: CharGrid -> [CharGrid] -- M.Map AugPos Distance --  DijkstraState AugPos (M.Map AugPos Distance)
modGrids charGrid = gridParts
 where
  gridParts = [A.listArray ((1, 1), (newH - 1, newW - 1)) [modifiedGrid ! (y, x) | y <- [offsetY .. offsetY + newH - 2], x <- [offsetX .. offsetX + newW - 2]] | offsetY <- [1, newH + 1], offsetX <- [1, newW + 1]]
  modifiedGrid = (charGrid A.// [((y, x), '@') | (y, x) <- neighbors8 center]) A.// [((y, x), '#') | (y, x) <- center : neighbors4 center]
  center = (newH + 1, newW + 1)
  newH = h `div` 2
  newW = w `div` 2
  (_, (h, w)) = A.bounds charGrid

solution2 :: CharGrid -> Distance -- M.Map AugPos Distance --  DijkstraState AugPos (M.Map AugPos Distance)
solution2 charGrid = foldr (\gridPart acc -> addDists acc (solution1 gridPart)) (Dist 0) gridParts
 where
  gridParts = [A.listArray ((1, 1), (newH, newW)) [modifiedGrid ! (y, x) | y <- [offsetY .. offsetY + newH], x <- [offsetX .. offsetX + newW]] | offsetY <- [1, newH + 1], offsetX <- [1, newW + 1]]
  modifiedGrid = (charGrid A.// [((y, x), '@') | (y, x) <- neighbors8 center]) A.// [((y, x), '#') | (y, x) <- center : neighbors4 center]
  center = (newH + 1, newW + 1)
  newH = h `div` 2
  newW = w `div` 2
  (_, (h, w)) = A.bounds charGrid

-- res :: CharGrid -> AugPos -> DijkstraState AugPos (M.Map AugPos Distance)
-- res charGrid end =
--   let
--     Just start = find (\pos -> charGrid ! pos == '@') $ A.indices charGrid
--    in
--     runDijkstra (makeLazyGraph charGrid) (start, 0) [end]
