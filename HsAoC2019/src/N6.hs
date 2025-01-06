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
