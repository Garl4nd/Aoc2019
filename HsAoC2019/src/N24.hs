module N24 (getSolutions24) where 

import Useful 
import Control.Monad.Trans.Reader
import Data.Array.ST
import Control.Monad.ST
import qualified Data.Array as A
import qualified Data.Map as M 
import qualified Data.Set as S 
import Data.Array ((!))

type Layout = A.Array GridPos CellState  
data CellState = Alive | Dead deriving (Eq, Show)
type AugPos = (Int, Int, Int) 

extNeighbors :: AugPos -> [AugPos]
extNeighbors (y0, x0, level) = concatMap remap ordinaryNeighbors where  
  ordinaryNeighbors = neighbors4 (y0, x0) 
  remap :: GridPos -> [AugPos]
  remap (y,x)  
    | x == 0 = [(3, 2, level +1)] 
    | x == 6 = [(3, 4, level +1)]
    | y == 0 = [(2, 3, level +1)]
    | y == 6 = [(4, 3, level +1)]
    | x == 3 && y == 3 && x0 == 2 = [(y, 1, level -1) | y <- [1..5]] 
    | x == 3 && y == 3 && x0 == 4 = [(y, 5, level -1) | y <- [1..5]] 
    | x == 3 && y == 3 && y0 == 2 = [(1, x, level -1) | x <- [1..5]] 
    | x == 3 && y == 3 && y0 == 4 = [(5, x, level -1) | x <- [1..5]] 
    | otherwise = [(y,x, level)]


neighborCounts :: S.Set AugPos -> M.Map AugPos Int 
neighborCounts = foldr (\pos accMap -> M.unionWith (+) accMap (M.fromList [(nei, 1) | nei <- extNeighbors pos ])) M.empty 

evolveCells :: S.Set AugPos -> S.Set AugPos 
evolveCells cells = M.foldrWithKey (\augPos count acc -> 
  if 
    | count == 1 -> S.insert augPos acc 
    | count == 2, augPos `S.notMember` cells -> S.insert augPos acc 
    | otherwise -> acc) S.empty (neighborCounts cells)    
  -- newCells = foldr (\)

newState :: Layout -> (GridPos, CellState)  -> CellState 
newState layout (pos, currentState) = let 
  liveNeighborCount = length [() | nei <- neighbors4 pos, inBounds nei, layout ! nei == Alive] 
  inBounds = A.inRange (A.bounds layout)
  in if 
    | currentState == Alive, liveNeighborCount /= 1 -> Dead  
    | currentState == Dead, liveNeighborCount `elem` [1,2] -> Alive
    | otherwise -> currentState 

findFirstDuplicate :: Eq a => [a] -> Maybe a 
findFirstDuplicate ls = go ls [] where 
  go [] _ = Nothing 
  go (x:xs) prevList = if x `elem` prevList then Just x else go xs (x:prevList )

score :: Layout -> Int 
score = foldr (\current accum -> let inc = if current == Alive then 1 else 0 in accum * 2 + inc ) 0 

caStep :: Layout -> Layout   
caStep layout = A.listArray (A.bounds layout) $ newState layout <$> A.assocs layout

parseFile :: String -> Layout 
parseFile = fmap (\c -> if c == '#' then Alive else Dead) . strToCharGrid

solution1 :: Layout -> Maybe Int 
solution1 initLayout = let 
  layouts = iterate caStep initLayout 
  scores = score <$> layouts 
  in findFirstDuplicate scores 

solution2 :: Layout -> Int 
solution2 layout = let
  initCells = foldr (\((y,x), state) acc -> if state == Alive then S.insert (y,x,0) acc else acc) S.empty $ A.assocs layout 
  ls = take 201 . map S.size $ iterate evolveCells initCells   
  in last ls

getSolutions24 = getSolutions parseFile solution1 solution2 
