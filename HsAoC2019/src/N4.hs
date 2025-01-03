module N4 () where

import Data.List (unfoldr)

checkNum :: Int -> Bool
checkNum num = any (uncurry (==)) adjs && all (uncurry (<=)) adjs
 where
  digits = digitsF num
  adjs = zip digits (drop 1 digits)

digitsF num = unfoldr (\num -> if num == 0 then Nothing else Just $ num `quotRem` 10) num
feasibleNums :: Int -> Int -> [Int]
feasibleNums l u = filter (checkNum) [l .. u]

-- >>> feasibleNums 125730 579381

-- >>> digitsF 4
