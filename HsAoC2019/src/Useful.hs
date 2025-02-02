{-# LANGUAGE FlexibleContexts #-}

module Useful (
  getSolutions,
  wordsWhen,
  splitOn,
  chunksOf,
  readStrList,
  splitBySubstr,
  writeXY,
  consecutivePairs,
  trimSpace,
  trimChar,
  countIf,
  count,
  groupBySorted,
  groupByUnique,
  pairChoices,
  pairVariations,
  strToCharGrid,
  charGridToStr,
  saveGridToFile,
  appendGridToFile,
  neighbors4,
  neighbors8,
  neighborsDiag,
  cycleDetectionFloyd,
  cycleDetectionBrent,
  fastMonoidIter,
  CharGrid,
  CharGridU,
  GridPos,
) where

import Control.Arrow
import Control.Monad ((>=>))
import qualified Data.Array.Unboxed as A
import Data.Bits
import Data.Function (on)
import Data.List (find, findIndex, groupBy, intercalate, isPrefixOf, sortOn, tails)
import Data.Maybe (fromJust)
import Data.Tuple (swap)

type GridPos = (Int, Int)
type CharGridU = A.UArray GridPos Char
type CharGrid = A.Array GridPos Char

wordsWhen :: (a -> Bool) -> [a] -> [[a]]
wordsWhen p s =
  case dropWhile p s of
    [] -> []
    s' -> w : wordsWhen p s''
     where
      (w, s'') = break p s'

splitOn :: (Eq a) => a -> [a] -> [[a]]
splitOn c = wordsWhen (== c)

trimIf :: (a -> Bool) -> [a] -> [a]
trimIf p = reverse . (dropWhile p) . reverse . (dropWhile p)

trimChar :: (Eq a) => a -> [a] -> [a]
trimChar c = trimIf (== c)

trimSpace :: String -> String
trimSpace = trimIf (== ' ')

readStrList :: (Read a, Num a) => Char -> String -> [a]
readStrList delim str = read <$> splitOn delim str

findSublist :: (Eq a) => [a] -> [a] -> Maybe Int
findSublist subList ls = findIndex (isPrefixOf subList) (tails ls)

splitBySubstr :: (Eq a) => [a] -> [a] -> [[a]]
splitBySubstr delim str =
  case findSublist delim str of
    Nothing -> [str]
    Just idx ->
      take idx str : splitBySubstr delim (drop (idx + length delim) str)

consecutivePairs :: [a] -> Maybe [(a, a)]
consecutivePairs (x0 : x1 : xs) = ((x0, x1) :) <$> consecutivePairs xs
consecutivePairs [_] = Nothing
consecutivePairs [] = Just []

chunksOf :: Int -> [a] -> [[a]]
chunksOf n ls = case splitAt n ls of
  ([], _) -> []
  (chunk, rest) -> chunk : chunksOf n rest

writeXY :: (Show a) => String -> [a] -> [a] -> IO ()
writeXY fileName xs ys = do
  writeFile fileName $
    intercalate "\n" $
      zipWith (\x y -> show x <> ", " <> show y) xs ys

groupByUnique :: (Ord b) => (a -> b) -> [a] -> [(a, Int)]
groupByUnique ordFunc ls = map (\grp -> (fst $ head grp, length grp)) (groupBy ((==) `on` snd) sortedList)
 where
  sortedList = sortOn snd $ map (\x -> (x, ordFunc x)) ls

-- groupBySorted :: (Ord b) => (a->b) -> [a] -> [([a],b)]
groupBySorted ordFunc ls = map (\grp -> (fst <$> grp, snd . head $ grp)) $ groupBy ((==) `on` snd) sortedList
 where
  sortedList = sortOn snd $ zip ls mappedList
  mappedList = ordFunc <$> ls

countIf :: (a -> Bool) -> [a] -> Int
countIf = (length .) . filter

count :: (Eq a) => a -> [a] -> Int
count match = countIf (== match)

pairChoices :: [a] -> [(a, a)]
-- pairChoices xs = concat $ zipWith (\a rest -> [(a, r) | r<-rest] ) xs (tail $ tails xs) -- [(xs !! i, xs !! j) | i <- [0 .. length xs - 1], j <- [i + 1 .. length xs - 1]]
pairChoices = concat . (zipWith (map . (,)) <*> (tail . tails)) -- just for point free fun, more comprehensible  implementations above
pairVariations :: [a] -> [(a, a)]
-- pairVariations xs = [(xs !! i, xs !! j) | i <- [0 .. length xs - 1], j <- [0 .. length xs - 1], i /= j]
pairVariations = ((++) <*> map swap) . pairChoices

strToCharGrid :: (A.IArray a Char) => String -> a GridPos Char
strToCharGrid file = A.listArray ((1, 1), (numLines, lineSize)) $ concat ls
 where
  ls = lines file
  numLines = length ls
  lineSize = length $ head ls

charGridToStr :: (A.IArray a Char) => a GridPos Char -> [String]
charGridToStr charGrid = [[charGrid A.! (y, x) | x <- [xmin .. xmax]] | y <- [ymin .. ymax]]
 where
  ((ymin, xmin), (ymax, xmax)) = A.bounds charGrid

saveGridToFile :: (A.IArray a Char) => String -> a GridPos Char -> IO ()
saveGridToFile filename charGrid =
  let
    content = intercalate "\n" $ charGridToStr charGrid
   in
    writeFile filename content

neighbors4 :: GridPos -> [GridPos]
neighbors4 (y, x) = [(y + a, x + b) | a <- [-1 .. 1], b <- [-1 .. 1], a == 0 && b /= 0 || a /= 0 && b == 0]

neighbors8 :: GridPos -> [GridPos]
neighbors8 (y, x) = [(y + a, x + b) | a <- [-1 .. 1], b <- [-1 .. 1], not (a == 0 && b == 0)]

neighborsDiag :: GridPos -> [GridPos]
neighborsDiag (y, x) = [(y + a, x + b) | a <- [-1, 1], b <- [-1, 1]]

appendGridToFile :: (A.IArray a Char) => String -> a GridPos Char -> IO ()
appendGridToFile filename charGrid =
  let
    content = intercalate "\n" $ charGridToStr charGrid
   in
    appendFile filename content

cycleDetectionFloyd :: (Eq a) => (a -> a) -> a -> (Int, Int)
cycleDetectionFloyd f x0 = (mu, lambda)
 where
  fiterates = iterate f x0
  repeatingPathOvershoot = findRepeats (drop 1 fiterates) (drop 2 fiterates)
  findRepeats (tortoise : tortoisePath) hareIterates@(hare : _ : harePath)
    | tortoise == hare = hareIterates
    | otherwise = findRepeats tortoisePath harePath
  repeatingPath = fmap (\(i, tortoise, _) -> (i, tortoise)) $ dropWhile (\(_, tortoise, hare) -> tortoise /= hare) $ zip3 [0 ..] fiterates repeatingPathOvershoot
  (mu, firstRepeat) = head repeatingPath
  lambda = (fst . head $ dropWhile ((/= firstRepeat) . snd) (drop 1 repeatingPath)) - mu

cycleDetectionBrent :: (Eq a) => (a -> a) -> a -> (Int, Int)
cycleDetectionBrent f x0 = (mu, lambda)
 where
  fiterates = iterate f x0
  lambda = lambdaLoop 1 1 x0 (drop 1 fiterates)
  lambdaLoop lam power tortoise (hare : restIterates)
    | tortoise == hare = lam
    | power == lam = lambdaLoop 1 (2 * power) hare restIterates
    | otherwise = lambdaLoop (lam + 1) power tortoise restIterates
  iteratesFromLambda = drop lambda fiterates
  mu = head [i | (i, tortoise, hare) <- zip3 [0 ..] fiterates iteratesFromLambda, tortoise == hare]

fastMonoidIter :: (Monoid f) => f -> Integer -> f
fastMonoidIter f n = go n f mempty
 where
  go k fPow accumFunc
    | k == 0 = accumFunc
    | otherwise = go (shiftR k 1) newFPow (if k .&. 1 == 1 then fPow <> accumFunc else accumFunc)
   where
    newFPow = fPow <> fPow

getSolutions :: (Show b, Show c) => (String -> a) -> (a -> b) -> (a -> c) -> (String -> IO (b, c))
getSolutions parser solution1 solution2 = readFile >=> (parser >>> (solution1 &&& solution2) >>> return)
