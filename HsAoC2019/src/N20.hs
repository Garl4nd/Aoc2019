{-# LANGUAGE TypeApplications #-}
module N20 (getSolutions20) where 
import Useful 
import GraphUtils 
import qualified Data.Array as A 
import Data.Array ((!))
import Data.Array.ST (runSTArray, MArray (newArray), writeArray)
import Control.Monad (forM_)
import Data.Char (isAsciiUpper)
import qualified Data.Map as M

data Tile = Free | Blocked | Portal String deriving (Show, Eq)
type TileArray = A.Array GridPos Tile 
type AugPos = (GridPos, Int)
makeGraph :: TileArray -> ArrayGraph GridPos 
makeGraph tileArray = A.array (A.bounds tileArray) $ makeEdges <$> A.assocs tileArray where 
  makeEdges (pos, val)
    | val == Free = (pos, [(nei, Dist 1) | nei <- freeNeighbors pos])
    | Portal portalName <- val = (pos, [(nei, Dist 1 ) | nei <- freeNeighbors pos ++ portalPartners pos portalName])   
    | otherwise = (pos, [])

  inBounds = A.inRange (A.bounds tileArray)  
  freeNeighbors pos = [nei | nei <- neighbors4 pos, inBounds nei, tileArray ! nei /= Blocked] 
  portalPartners portalPos portalName =  [portalPos' | (portalPos', Portal portalName') <- A.assocs tileArray, portalPos /= portalPos', portalName == portalName'] 

-- data LazyGraph ctx node = LazyGraph {nodes :: ctx, edgeFunc :: node -> Edges node, bounds :: (node, node)}
makeGraph2 :: TileArray -> LazyGraph AugPos 
makeGraph2 tileArray = LazyGraph {edgeFunc = getEdges, bounds = ((minBounds, 0), (maxBounds, 200))} where 
  bounds@(minBounds, maxBounds@(yMax, xMax)) = A.bounds tileArray 
  inBounds = A.inRange bounds  
  freeNeighbors pos = [nei | nei <- neighbors4 pos, inBounds nei, tileArray ! nei /= Blocked] 
  isOuterLevel (y,x) = x ==3 || y == 3 || x == xMax -2 || y == yMax -2
  getEdges augPos@(pos, level)
    | tileArray ! pos == Free = [((nei, level), Dist 1) | nei <- freeNeighbors pos]
    | Portal portalName <- tileArray ! pos = [((nei, level), Dist 1) | nei <- freeNeighbors pos] ++ 
      (if level == 0 && isOuterLevel pos then []  else [((nei, level + levelDif), Dist 1) | (nei, levelDif) <- portalPartnersAug pos portalName  ])
  portalPartnersAug portalPos portalName =  [(portalPos', if isOuterLevel portalPos then -1 else 1) | (portalPos', Portal portalName') <- A.assocs tileArray, portalPos /= portalPos', portalName == portalName'] 

parseFile :: String -> TileArray 
parseFile file = let 
  charGrid = strToCharGrid file
  inBounds = A.inRange (A.bounds charGrid)
  isEdge pos = not  (inBounds pos) || charGrid ! pos == ' '
  isLabel pos = inBounds pos && isAsciiUpper (charGrid ! pos)
  in runSTArray $ do 
    tileAr <- newArray (A.bounds charGrid) Blocked 
    forM_ (A.assocs charGrid ) $
      \(pos@(y,x), val) -> do 
       if val == '.' then  
        writeArray tileAr pos $ 
         if 
          | isLabel (y+1, x) && isLabel (y+2, x) ->  Portal [charGrid ! (y+1,x), charGrid ! (y+2,x)] 
          | isLabel (y-1, x) && isLabel (y-2, x) ->  Portal [charGrid ! (y-2,x), charGrid ! (y-1,x)] 
          | isLabel (y, x-2) && isLabel (y, x-1) ->  Portal [charGrid ! (y,x-2), charGrid ! (y,x-1)] 
          | isLabel (y, x+1) && isLabel (y, x+2) -> Portal [charGrid ! (y,x+1), charGrid ! (y,x+2)] 
          | otherwise ->  Free   
       else writeArray tileAr pos Blocked  
    return tileAr
        

shortestPath :: TileArray -> Distance 
shortestPath tileAr = let 
  graph = makeGraph tileAr 
  [startPos] = [pos | (pos, Portal "AA") <- A.assocs tileAr] 
  [endPos] =   [pos | (pos, Portal "ZZ") <- A.assocs tileAr] 
  in (distanceMap $ runDijkstra graph startPos [endPos]) ! endPos 

shortestPath2 :: TileArray -> Distance 
shortestPath2 tileAr = let 
  graph = makeGraph2 tileAr 
  [startPos] = [pos | (pos, Portal "AA") <- A.assocs tileAr] 
  [endPos] =   [pos | (pos, Portal "ZZ") <- A.assocs tileAr] 
  in (distanceMap $ runDijkstra @(A.Array AugPos Distance) graph  (startPos, 0 :: Int) [(endPos,0)]) ! (endPos, 0) 

getSolutions20 = getSolutions parseFile shortestPath shortestPath2
