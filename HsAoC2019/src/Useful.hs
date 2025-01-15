{-# LANGUAGE FlexibleContexts #-}

module Useful (
  getSolutions,
  wordsWhen,
  splitOn,
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
  cycleDetectionFloyd',
  CharGrid,
  CharGridU,
  GridPos,
) where

import Control.Arrow
import Control.Monad ((>=>))
import qualified Data.Array.Unboxed as A
import Data.Function (on)
import Data.List (findIndex, groupBy, intercalate, isPrefixOf, sortOn, tails, find)
import Data.Tuple (swap)
import Data.Maybe (fromJust)

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

cycleDetectionFloyd :: Eq a => [a]  -> (Int, Int)
cycleDetectionFloyd fIterates = (mu, lambda) where    
  nu = fromJust $ find (\i -> fIterates !! i == fIterates !! (2*i)) [1..]  
  mu = fromJust $ find (\i -> fIterates !! i == fIterates !! (i+nu)) [0..]
  lambda = fromJust $ find (\i -> fIterates !! mu == fIterates !! (mu+i )) [1..]

cycleDetectionFloyd' :: Eq a => (a->a ) -> a  -> (Int, Int)
cycleDetectionFloyd' f x0 = (mu, lambda) where    
  f2 = f . f 
  fiterates = iterate f x0 
  f2iterates = iterate f2 x0 
  repeatingEl = head [tortoise | (tortoise, hare) <- zip (drop 1 fiterates) (drop 1 f2iterates), tortoise == hare]   
  repeatingPart = dropWhile ((/= repeatingEl).snd) $ zip [0..] fiterates
  mu = fst . head $ repeatingPart  
  lambda = (fst . head $ dropWhile ((/= repeatingEl).snd) (drop 1 repeatingPart)) - mu  



getSolutions :: (Show b, Show c) => (String -> a) -> (a -> b) -> (a -> c) -> (String -> IO (b, c))
getSolutions parser solution1 solution2 = readFile >=> (parser >>> (solution1 &&& solution2) >>> return)
