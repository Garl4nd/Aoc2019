module N4 (getSolutions4) where

import Data.List (nub, unfoldr)
import Data.Tuple (swap)
import Useful (getSolutions)

checkNum1 :: Int -> Bool
checkNum1 num = any (uncurry (==)) adjs && all (uncurry (<=)) adjs
  where
    digits = reverse . unfoldr (\num -> if num == 0 then Nothing else Just $ swap $ num `quotRem` 10) $ num
    adjs = zip digits (drop 1 digits)

checkNum2 :: Int -> Bool
checkNum2 num = singlePairExists digits && all (uncurry (<=)) adjs
  where
    digits = reverse . unfoldr (\num -> if num == 0 then Nothing else Just $ swap $ num `quotRem` 10) $ num
    adjs = zip digits (drop 1 digits)
    singlePairExists [a, b, c, d, e, f] =
      a == b && b /= c
        || b == c && a /= b && c /= d
        || c == d && b /= c && d /= e
        || d == e && c /= d && e /= f
        || e == f && d /= e

feasibleNums p l u = filter p [l .. u]

solution1' = (length .) . feasibleNums checkNum2

solution2' = (length .) . feasibleNums checkNum2

-- genAllNumSeqs :: Int -> Int -> [[Int]]
-- genAllNumSeqs places fixedPlace = go 1 0
--  where
--   go :: Int -> Int -> [[Int]]
--   go currentPlace currentVal
--     | currentPlace > places = [[]]
--     | currentPlace == places = [[val] | val <- [currentVal .. 9]]
--     | otherwise = [val : rest | val <- [currentVal .. 9], rest <- go (currentPlace + 1) 0]
--
-- genAllNums :: Int -> Int -> [Int]
-- genAllNums places fixedPlace = go 1 0
--  where
--   go :: Int -> Int -> [Int]
--   go currentPlace currentVal
--     | currentPlace >= places = [currentVal .. 9]
--     | otherwise = [val + 10 * rest | val <- [currentVal .. 9], rest <- go (currentPlace + 1) 0]
--
-- genNums1F :: Int -> Int -> [Int]
-- genNums1F places fixedPlace = if places == 0 then [] else go 0 9
--  where
--   go :: Int -> Int -> [Int]
--   go currentPlace currentMax
--     | currentPlace == places - 1 = allowedRange
--     | otherwise = [val + 10 * rest | val <- allowedRange, rest <- go (currentPlace + 1) val]
--    where
--     allowedRange = if currentPlace == fixedPlace then [currentMax] else [1 .. currentMax]
--
genNums1F :: Int -> Int -> [Int]
genNums1F places fixedPlace = go 1 9
  where
    go :: Int -> Int -> [Int]
    go currentPlace currentMax
      | currentPlace > places = [0]
      | currentPlace == places = [0 .. currentMax]
      | currentPlace == fixedPlace = [11 * n + 100 * rest | n <- [1 .. currentMax], rest <- go (currentPlace + 2) n]
      | otherwise = [val + 10 * rest | val <- [1 .. currentMax], rest <- go (currentPlace + 1) val]

genNums2F :: Int -> Int -> [Int]
genNums2F places fixedPlace = go 1 9
  where
    go :: Int -> Int -> [Int]
    go currentPlace currentMax
      | currentPlace > places = [0]
      | currentPlace == places = [0 .. currentMax]
      | currentPlace == fixedPlace = [11 * n + 100 * rest | n <- [1 .. if currentPlace == 1 then currentMax else currentMax - 1], rest <- go (currentPlace + 2) (n - 1)]
      | otherwise = [val + 10 * rest | val <- [1 .. currentMax], rest <- go (currentPlace + 1) val]

genNums1 places = concat [genNums1F places fixed | fixed <- [1 .. places - 1]]

genNums2 places = concat [genNums2F places fixed | fixed <- [1 .. places - 1]]

countUniqueNums f l r = length . nub $ [num | num <- f 6, l <= num && num <= r]

solution1 (l, r) = countUniqueNums genNums1 l r

solution2 (l, r) = countUniqueNums genNums2 l r

getSolutions4 = getSolutions (const (125730, 579381)) solution1 solution2

-- >>> feasibleNums 125730 579381
