{-# LANGUAGE NumericUnderscores #-}

module N19 (getSolutions19) where

import Data.Function.Memoize (memoize)
import Data.List (find)
import Data.Maybe (fromJust)
import IntCode
import Useful

getBorders :: [Int] -> Int -> (Int, Int, Int)
getBorders code yC =
  let
    getX :: Int -> Int
    getX y = round (0.65 * fromIntegral y) -- 0.65 comes from the trend estimated in ghci
    responses y xC = [(x, head . machineOutputs . runCodeWInputST [x, y] $ code) | x <- [xC - 150 .. xC + 150]]
    getBeamLocs y = [x | (x, 1) <- responses y (getX y)]
    borders y = let beamLocs = getBeamLocs y in (y, minimum beamLocs, maximum beamLocs)
   in
    borders yC

binarySearch :: (Integral a) => (a -> Bool) -> a -> a -> a
binarySearch f x0 x1
  | f x0 = x0
  | x0 + 1 == x1 = x1
  | otherwise =
      let m = (x0 + x1) `div` 2
       in if f m
            then binarySearch f x0 m
            else binarySearch f m x1

findSquare :: [Int] -> Int -> Int -> Maybe (Int, Int)
findSquare code yL yU =
  let
    getBordersM = memoize (getBorders code)
    lB y = let (_, res, _) = getBordersM y in res
    uB y = let (_, _, res) = getBordersM y in res
    p y = lB (y + 99) + 99 <= uB y
    binSerchRes = binarySearch p yL yU
    detailedSearch = find p [binSerchRes - 30 .. binSerchRes + 30]
   in
    fmap (\yRes -> (lB (yRes + 99), yRes)) detailedSearch

drawSpaceAround :: [Int] -> Int -> Int -> IO ()
drawSpaceAround code xS yS =
  let
    inputs = [[xS + x, yS + y] | y <- [0 .. 150], x <- [0 .. 200]]
    tractMap = [(input, head $ machineOutputs (runCodeWInputST input code)) | input <- inputs]
    charRes = map (\(_, c) -> if c == 1 then '#' else '.') tractMap
   in
    writeFile "outputs/tractorMap.txt" $ unlines $ chunksOf 201 charRes

solution1 :: [Int] -> Int
solution1 code = countIf (== 1) [head . machineOutputs . runCodeWInputST [x, y] $ code | y <- [0 .. 49], x <- [0 .. 49]]

solution2 :: [Int] -> Int
solution2 code = let Just (x, y) = findSquare code 500 3000 in 10_000 * x + y

getSolutions19 = getSolutions codeParser solution1 solution2
