{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}

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

makeLazyGraph :: CharGrid -> LazySetGraph
makeLazyGraph grid = LazyGraph{nodes = grid, edgeFunc = getAugEdges, bounds = ((minBound, 0), (maxBound, shiftL 1 26))}
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
      | let val = grid ! n, isAsciiUpper val = (keys, if keys .&. (encode $ toLower val) == 0 then Inf else Dist 1)
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

solution :: CharGrid -> Distance -- M.Map AugPos Distance --  DijkstraState AugPos (M.Map AugPos Distance)
solution charGrid =
  let
    Just start = find (\pos -> charGrid ! pos == '@') $ A.indices charGrid
    keys = [pos | pos <- A.indices charGrid, isAsciiLower (charGrid ! pos)]
    letters = [val | val <- A.elems charGrid, isAsciiLower val]
    maxVal :: Int = shiftL (encode (maximum letters)) 1 - 1
    targets = [(key, maxVal) | key <- keys]
    distMap = distanceMap $ runDijkstra @(M.Map AugPos Distance) (makeLazyGraph charGrid) (start, 0) targets
   in
    minimum $ [val | pos <- targets, let val = fromMaybe Inf $ M.lookup pos distMap]

res :: CharGrid -> AugPos -> DijkstraState AugPos (M.Map AugPos Distance)
res charGrid end =
  let
    Just start = find (\pos -> charGrid ! pos == '@') $ A.indices charGrid
   in
    runDijkstra (makeLazyGraph charGrid) (start, 0) [end]
