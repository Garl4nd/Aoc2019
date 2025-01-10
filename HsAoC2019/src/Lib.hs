module Lib (
  maybeSolver,
) where

import N1
import N2
import N3
import N4
import N5
import N6
import N7

import N9

maybeSolver :: Int -> Maybe (String -> IO (Int, Int))
maybeSolver day = case day of
  1 -> Just getSolutions1
  2 -> Just getSolutions2
  3 -> Just getSolutions3
  4 -> Just getSolutions4
  5 -> Just getSolutions5
  6 -> Just getSolutions6
  7 -> Just getSolutions7
  -- 8 -> Just getSolutions8 -- 9 -> Just $ getSolutions9 "inputs/9.txt"
  9 -> Just getSolutions9 -- 9 -> Just $ getSolutions9 "inputs/9.txt"
  -- 10 -> Just getSolutions10
  -- 11 -> Just getSolutions11
  -- 12 -> Just getSolutions12
  -- 13 -> Just getSolutions13
  -- 14 -> Just getSolutions14
  -- 15 -> Just getSolutions15
  -- 16 -> Just getSolutions16
  -- 17 -> Just getSolutions17
  -- 18 -> Just $ \filename -> do
  --   (a, b) <- getSolutions18 filename
  --   let [bx, by] = splitOn ',' b
  --   return (a, read $ bx <> by)
  -- 19 -> Just getSolutions19
  -- 20 -> Just getSolutions20
  -- 21 -> Just getSolutions21
  -- 22 -> Just getSolutions22
  -- 23 -> Just getSolutions23
  -- 24 -> Just getSolutions24
  -- 25 -> Just getSolutions25
  _ -> Nothing
