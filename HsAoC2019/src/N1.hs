module N1 (getSolutions1)
where

import Useful (getSolutions)

fuel :: Int -> Int
fuel = subtract 2 . (`div` 3)
solution1 :: [Int] -> Int
solution1 = sum . map fuel

solution2 :: [Int] -> Int
solution2 = sum . map (sum . takeWhile (> 0) . drop 1 . iterate fuel)

getSolutions1 = getSolutions (map read . lines) solution1 solution2
