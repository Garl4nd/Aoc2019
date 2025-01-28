{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilyDependencies #-}

module GraphUtils (runDijkstraSTG, bronKerbosch, addDist, distanceToInt, bestPaths, LabeledGraph, ArrayGraph, Distance (Dist, Inf), Num, DistanceMap, Path) where

-- , unsafeThaw)
import Control.Monad (forM, forM_)
import Control.Monad.ST
import Data.Array ((!))
import qualified Data.Array as A
import Data.Array.ST (STArray, newArray, readArray, runSTArray, writeArray)
import qualified Data.Heap as H
import Data.List (sortOn)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.STRef
import qualified Data.Set as S
import GHC.List (foldl')

type NumType = Int
type Edges node = [(node, NumType)]
type DistanceMap node = A.Array node Distance
type ArrayGraph node = A.Array node (Edges node)
data Distance = Dist NumType | Inf deriving (Eq, Show)

-- type Distance = NumType
distanceToInt :: Distance -> Int
distanceToInt Inf = 1000000000000000
distanceToInt (Dist n) = n

class UnlabeledGraph graph node where
  getEdgesU :: graph -> node -> [node]
  getBoundsU :: graph -> (node, node)
  verticesU :: graph -> S.Set node

instance (A.Ix node) => UnlabeledGraph (A.Array node [node]) node where
  verticesU = S.fromList . A.indices
  getEdgesU = (!)
  getBoundsU = A.bounds
instance (Ord node) => UnlabeledGraph (M.Map node [node]) node where
  verticesU = S.fromList . M.keys
  getEdgesU = (M.!)
  getBoundsU mapGraph = (minimum keys, maximum keys) where keys = M.keys mapGraph

class (Ord node) => LabeledGraph graph node where
  getEdges :: graph -> node -> Edges node
  getBounds :: graph -> (node, node)
  vertices :: graph -> S.Set node

instance (A.Ix node) => LabeledGraph (A.Array node (Edges node)) node where
  getEdges = (!) -- graphAr ! node
  getBounds = A.bounds
  vertices = S.fromList . A.indices

instance (A.Ix node) => LabeledGraph (M.Map node (Edges node)) node where
  getEdges = (M.!) -- graphAr ! node
  vertices = S.fromList . M.keys
  getBounds mapGraph = (minimum keys, maximum keys) where keys = M.keys mapGraph

data LazyGraph ctx node f = LazyGraph {nodes :: ctx node, edgeFunc :: ctx node -> node -> Edges node, bounds :: (node, node)}
instance (A.Ix node) => LabeledGraph (LazyGraph ctx node f) node where
  getEdges LazyGraph{nodes, edgeFunc} = edgeFunc nodes
  getBounds = bounds
  vertices = const S.empty

instance Ord Distance where
  (<=) :: Distance -> Distance -> Bool
  Inf <= Inf = True
  Inf <= Dist _ = False
  Dist _ <= Inf = True
  Dist x <= Dist y = x <= y

addDist :: Distance -> NumType -> Distance
addDist (Dist x) y = Dist (x + y)
addDist _ _ = Inf

--
-- data DijkstraState node = DijkstraState
--   { finalStates :: S.Set node
--   , distanceMap :: DistanceMap node
--   , nodeQueue :: H.MinPrioHeap Distance node
--   , dests :: S.Set node
--   }
--   deriving (Show)
--
-- data DijkstraStateST node s = DijkstraStateST
--   { finalStatesST :: S.Set node
--   , distanceMapST :: DistanceMapST node s
--   , nodeQueueST :: H.MinPrioHeap Distance node
--   , destsST :: S.Set node
--   }
--
class DistanceMapClass dm node where
  type STDistMap s node dm = r | r -> node dm
  new :: (node, node) -> ST s (STDistMap s node dm)
  getVal :: dm -> node -> Distance
  readVal :: STDistMap s node dm -> node -> ST s Distance
  writeVal :: STDistMap s node dm -> node -> Distance -> ST s ()
  unST :: (forall s. ST s (STDistMap s node dm)) -> dm

instance (A.Ix node) => DistanceMapClass (A.Array node Distance) node where
  type STDistMap s node (A.Array node Distance) = STArray s node Distance
  new = (`newArray` Inf)
  getVal = (!)
  readVal = readArray
  writeVal = writeArray
  unST = runSTArray

instance (Ord node) => DistanceMapClass (M.Map node Distance) node where
  type STDistMap s node (M.Map node Distance) = STRef s (M.Map node Distance)
  new = const $ newSTRef M.empty
  getVal = (M.!)
  readVal distMapRef at = fromMaybe Inf . M.lookup at <$> readSTRef distMapRef
  writeVal distMapRef at val = modifySTRef distMapRef (M.insert at val)
  unST stCalc = runST $ stCalc >>= readSTRef

data (DistanceMapClass dm node) => DijkstraStateG node dm = DijkstraStateG
  { finalStatesG :: S.Set node
  , distanceMapG :: dm
  , nodeQueueG :: H.MinPrioHeap Distance node
  , destsG :: S.Set node
  }

data (DistanceMapClass dm node) => DijkstraStateSTG node dm s = DijkstraStateSTG
  { finalStatesSTG :: S.Set node
  , distanceMapSTG :: STDistMap s node dm
  , nodeQueueSTG :: H.MinPrioHeap Distance node
  , destsSTG :: S.Set node
  }

-- dijkstraLoop :: forall node graph s. (Show node, LabeledGraph graph node) => graph -> ST s (DijkstraStateST node s) -> ST s (DijkstraStateST node s)
-- dijkstraLoop graph dsST = do
--   ds@DijkstraStateST{finalStatesST = fs, distanceMapST = dm, nodeQueueST = nq, destsST = dsts} <- dsST
--   if S.null dsts
--     then
--       dsST -- return ds
--     else case H.view nq of
--       Nothing -> dsST -- return ds
--       Just (distNode, nq') -> processNode distNode
--        where
--         processNode :: (Distance, node) -> ST s (DijkstraStateST node s)
--         processNode (_, minNode)
--           | S.member minNode fs = dijkstraLoop graph (return $ ds{nodeQueueST = nq'})
--         processNode (dist, minNode) = do
--           let candidateList = [(neighbor, newDist) | (neighbor, edgeVal) <- getEdges graph minNode, S.notMember neighbor fs, let newDist = addDist dist edgeVal]
--           currentDists <- forM candidateList (\(neigh, _) -> readArray dm neigh)
--           let updateList = [(neighbor, newDist) | ((neighbor, newDist), currentDist) <- zip candidateList currentDists, newDist <= currentDist]
--               updatedQueue = foldl' (\q (node, dist') -> H.insert (dist', node) q) nq' updateList
--           forM_ updateList $ \(pos, newDist) -> writeArray dm pos newDist
--           dijkstraLoop graph (return $ ds{finalStatesST = S.insert minNode fs, distanceMapST = dm, nodeQueueST = updatedQueue, destsST = S.delete minNode dsts})
--
-- runDijkstraST :: forall graph node. (Show node, LabeledGraph graph node) => graph -> node -> [node] -> DijkstraState node
-- runDijkstraST graph start ends = extractFromST $ dijkstraLoop graph initState
--  where
--   extractFromST :: (forall s. ST s (DijkstraStateST node s)) -> DijkstraState node
--   extractFromST stRes = DijkstraState{finalStates = fs, distanceMap = dm, nodeQueue = nq, dests = dst}
--    where
--     dm = runSTArray $ distanceMapST <$> stRes
--     (fs, nq, dst) = runST $ do
--       DijkstraStateST{destsST = dst, nodeQueueST = nq', finalStatesST = fs'} <- stRes
--       return (fs', nq', dst)
--   initState :: ST s (DijkstraStateST node s)
--   initState = do
--     stAr <- newArray (getBounds graph) Inf
--     writeArray stAr start $ Dist 0
--     return DijkstraStateST{finalStatesST = S.empty, distanceMapST = stAr, nodeQueueST = H.singleton (Dist 0, start), destsST = S.fromList ends} -- runDijkstraSTLow graph start ends
--
dijkstraLoopG :: forall node graph s dstMap. (Show node, LabeledGraph graph node, DistanceMapClass dstMap node) => graph -> ST s (DijkstraStateSTG node dstMap s) -> ST s (DijkstraStateSTG node dstMap s)
dijkstraLoopG graph dsST = do
  ds@DijkstraStateSTG{finalStatesSTG = fs, distanceMapSTG = dm, nodeQueueSTG = nq, destsSTG = dsts} <- dsST
  if S.null dsts
    then
      dsST -- return ds
    else case H.view nq of
      Nothing -> dsST -- return ds
      Just (distNode, nq') -> processNode distNode
       where
        processNode :: (Distance, node) -> ST s (DijkstraStateSTG node dstMap s)
        processNode (_, minNode)
          | S.member minNode fs = dijkstraLoopG graph (return $ ds{nodeQueueSTG = nq'})
        processNode (dist, minNode) = do
          let candidateList = [(neighbor, newDist) | (neighbor, edgeVal) <- getEdges graph minNode, S.notMember neighbor fs, let newDist = addDist dist edgeVal]
          currentDists <- forM candidateList (\(neigh, _) -> readVal dm neigh)
          let updateList = [(neighbor, newDist) | ((neighbor, newDist), currentDist) <- zip candidateList currentDists, newDist <= currentDist]
              updatedQueue = foldl' (\q (node, dist') -> H.insert (dist', node) q) nq' updateList
          forM_ updateList $ \(pos, newDist) -> writeVal dm pos newDist
          dijkstraLoopG graph (return $ ds{finalStatesSTG = S.insert minNode fs, distanceMapSTG = dm, nodeQueueSTG = updatedQueue, destsSTG = S.delete minNode dsts})

runDijkstraSTG :: forall graph node dm. (Show node, LabeledGraph graph node, DistanceMapClass dm node) => graph -> node -> [node] -> DijkstraStateG node dm
runDijkstraSTG graph start ends = extractFromST $ dijkstraLoopG graph initState
 where
  extractFromST :: (forall s. ST s (DijkstraStateSTG node dm s)) -> DijkstraStateG node dm
  extractFromST stRes = DijkstraStateG{finalStatesG = fs, distanceMapG = dmRes, nodeQueueG = nq, destsG = dst}
   where
    dmRes = unST $ distanceMapSTG <$> stRes
    (fs, nq, dst) = runST $ do
      DijkstraStateSTG{destsSTG = dst, nodeQueueSTG = nq', finalStatesSTG = fs'} <- stRes
      return (fs', nq', dst)
  initState :: ST s (DijkstraStateSTG node dm s)
  initState = do
    distMapSt <- new (getBounds graph)
    writeVal distMapSt start $ Dist 0
    return DijkstraStateSTG{finalStatesSTG = S.empty, distanceMapSTG = distMapSt, nodeQueueSTG = H.singleton (Dist 0, start), destsSTG = S.fromList ends} -- runDijkstraSTLow graph start ends

type Path node = [node]

bestPathsAr :: forall graph node. (Show node, A.Ix node, LabeledGraph graph node) => graph -> node -> node -> [Path node]
bestPathsAr = bestPaths @(A.Array node Distance)

bestPaths :: forall dm graph node. (Show node, LabeledGraph graph node, DistanceMapClass dm node) => graph -> node -> node -> [Path node]
bestPaths graph start end = if getVal distMap end == Inf then [] else reverse <$> go end
 where
  go :: node -> [Path node]
  go pos
    | pos == start = [[pos]]
    | otherwise =
        let
          neighbors = getEdges reversedGraph pos
          departureNodes = [node | (node, val) <- neighbors, addDist (getVal distMap node) val == getVal distMap pos]
         in
          [pos : path | path <- concatMap go departureNodes]
  reversedGraph = graph -- actually reverse for directed graphs
  distMap :: dm
  distMap = distanceMapG $ runDijkstraSTG graph start [end] -- getCompleteDistMap ar

bronKerbosch :: forall graph node. (UnlabeledGraph graph node, Ord node) => graph -> S.Set (S.Set node)
bronKerbosch graph = go S.empty (verticesU graph) S.empty
 where
  go :: S.Set node -> S.Set node -> S.Set node -> S.Set (S.Set node)
  go sR sP sX
    | S.null sP && S.null sX = S.singleton sR
    | otherwise = cliques
   where
    pivot = last $ sortOn (length . getEdgesU graph) $ S.toList $ S.union sP sX
    reducedSet = sP S.\\ nei pivot -- S.fromList (getEdgesU graph pivot)
    nei = S.fromList . getEdgesU graph
    (cliques, _, _) = foldr update (S.empty, sP, sX) reducedSet
    update v (cliques', sP', sX') = (cliques'', sP'', sX'')
     where
      cliques'' = S.union cliques' $ go (S.insert v sR) (sP' `S.intersection` nei v) (sX' `S.intersection` nei v)
      sP'' = S.delete v sP'
      sX'' = S.insert v sX'

-- g1,g2 :: A.Array Int [Int]
-- g1 = A.listArray (1,7) [[2,5], [3,5,1], [2,4], [3,5,6, 7],[1,2,4],[4,7], [4,6]]
-- g2 = A.listArray (1,4) [[2,4], [3,1], [4,2], [1,3]]
-- ex1 :: S.Set (S.Set Int)
-- ex1 = bronKerbosch g1
-- ex2 :: S.Set (S.Set Int)
-- ex2 = bronKerbosch g2
