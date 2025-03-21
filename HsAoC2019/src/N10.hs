module N10 (getSolutions10) where

import qualified Data.Array as A
import Data.Foldable (Foldable (toList), maximumBy)
import Data.Function (on)
import Data.List (find,  sortBy)
import qualified Data.Map as M
import qualified Data.Set as S
import Useful

parseFile :: String -> CharGrid
parseFile = strToCharGrid

getAsteroids :: CharGrid -> [GridPos]
getAsteroids charGrid = [idx | (idx, val) <- A.assocs charGrid, val == '#']

type Line = (Int, Int)
calcLine :: GridPos -> GridPos -> Line
calcLine (y, x) (y', x') =
  let
    dy = y' - y
    dx = x' - x
    d = gcd dy dx
   in
    (dy `div` d, dx `div` d)

losCounter :: CharGrid -> M.Map GridPos (S.Set GridPos)
losCounter charGrid =
  let
    asts = getAsteroids charGrid
    astLines ast = [calcLine ast ast' | ast' <- asts, ast' /= ast]
    countVisible = S.fromList . astLines
   in
    foldr (M.insert <*> countVisible) M.empty asts

solution1 :: CharGrid -> Int
solution1 = maximum . fmap S.size . losCounter

angleComparison:: GridPos -> GridPos -> Ordering
angleComparison (y,x) (y',x') 
  | x >= 0 && x' < 0 = LT  
  | x < 0 && x' >= 0 = GT 
  | otherwise = let crossP = -(x*y') + y * x' in 
    if 
      | crossP < 0 -> LT 
      | crossP > 0 -> GT 
      | y > 0 && y' <0 -> GT 
      | y <0 && y' >0 -> LT 
      | otherwise -> EQ 

solution2 :: CharGrid -> Int
solution2 charGrid =
  let
    asteroids = getAsteroids charGrid
    ((y, x), lineSet ) = maximumBy (compare `on` S.size . snd) $ M.assocs $ losCounter charGrid
    -- sortedLines = sortby (negate . uncurry atan2 . \(y, x) -> (fromIntegral x, fromIntegral y)) $ toList monitoringStation
    sortedLines = sortBy angleComparison $ toList lineSet 
    (dy, dx) = sortedLines !! 199 
    Just (yRes, xRes) = find (`elem` asteroids) [(y + k * dy, x + k * dx) | k <- [1 ..]]
   in
    100 * (xRes - 1) + yRes - 1

getSolutions10 = getSolutions parseFile solution1 solution2
