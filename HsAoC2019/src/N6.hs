module N6 () where

import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import Data.Char (isAlphaNum)
import Data.Either (fromRight)
import qualified Data.Map as M
import Data.Void (Void)
import Text.Megaparsec (MonadParsec (takeWhile1P, takeWhileP), Parsec, endBy, runParser, sepBy, sepEndBy, some)
import Text.Megaparsec.Char (alphaNumChar, char, letterChar, newline, string)
import Text.Megaparsec.Debug
import Data.List (unfoldr)
import Useful (getSolutions)

type SParser = Parsec Void String
type Graph = M.Map String [String]
fileParser :: StateT Graph SParser [()]
fileParser = sepEndBy orbitParser newline
 where
  orbitParser = do
    graph <- get
    key <- lift $ takeWhile1P Nothing isAlphaNum <* char ')'
    val <- lift $ takeWhile1P Nothing isAlphaNum
    put $ M.insertWith (++) key [val] graph
parseFile :: String -> Graph
parseFile = fromRight M.empty . runParser (execStateT fileParser M.empty) ""

getDistanceMap :: Graph -> String -> M.Map String Int
getDistanceMap graph = go 0
 where
  go :: Int -> String -> M.Map String Int
  go dist currentKey = M.insert currentKey dist lowerMap
   where
    lowerMap = case M.lookup currentKey graph of
      Nothing -> M.empty
      Just edges -> M.unions (go (dist + 1) <$> edges)

reverseGraph :: Graph -> Graph
reverseGraph = M.foldrWithKey (\key edges accMap -> foldr (\val newMap -> M.insertWith (++) val [key]  newMap) (M.insertWith (++) key [] accMap) edges) M.empty

loeb :: (Functor f) => f (f a -> a) -> f a
loeb fs = go where go = fmap ($ go) fs

type DistMap = M.Map String Int

graphUpdates :: Graph -> M.Map String (DistMap -> Int)
graphUpdates = M.mapWithKey updateFunc
 where
  updateFunc key edges distMap = case edges of
    [] -> 0
    target : _ -> succ $ distMap M.! target

getDistanceMap' :: Graph -> M.Map String Int
getDistanceMap' graph = loeb (graphUpdates $ reverseGraph  graph)

getCommonAncestorLength :: String -> String -> Graph -> Int 
getCommonAncestorLength node1 node2 graph = go node1 node2 where   
  rGraph = reverseGraph graph
  path  = unfoldr (\n -> (\case  {[] -> Nothing; (next:_) -> Just  (n, next)}) =<<  M.lookup n rGraph)
  go n1 n2 = let 
    path1 = zip [0..] $ path n1 
    path2 = zip [0..] $ path n2 
    commonNodeDists = [l1 + l2  | (l1, n1) <- path1, (l2, n2) <- path2, n1 == n2] 
    in case commonNodeDists of 
      firstDist:_ -> firstDist - 2  
      _ -> 0 

solution1 = sum .  getDistanceMap' 
solution2 = getCommonAncestorLength "SAN" "YOU"
getSolutions6 = getSolutions parseFile solution1 solution2 
