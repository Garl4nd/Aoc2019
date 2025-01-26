module N17 () where

import Control.Arrow
import Data.Char
import Data.List (elemIndex, findIndex)
import Data.Maybe (isJust)

stripPrefix :: (Eq a) => [a] -> [a] -> Maybe [a]
stripPrefix ls [] = Just ls
stripPrefix [] (p : _) = Nothing
stripPrefix (x : xs) (p : rest) = if x == p then stripPrefix xs rest else Nothing

compositePatterns :: forall a. (Eq a) => [a] -> Int -> Int -> [[[a]]]
compositePatterns ls maxLength maxPatternNum = go ls []
 where
  go :: [a] -> [[a]] -> [[[a]]]
  go [] _ = [[]]
  go rem foundPats = [pattern : remCombinations | (pattern, Just rest) <- (id &&& stripPrefix rem) <$> foundPats, remCombinations <- go rest foundPats] ++ newRes
   where
    newRes =
      if length foundPats >= maxPatternNum
        then []
        else
          [newPattern : remCombinations | n <- [1 .. min (length rem) maxLength], let (newPattern, rest) = splitAt n rem, remCombinations <- go rest (newPattern : foundPats)]

patternsToNumseq :: (Eq a) => [[a]] -> [Char]
patternsToNumseq pats = (\i -> chr $ (ord 'A') + i) <$> go [] pats
 where
  go _ [] = []
  go addedPats (p : rest) = case elemIndex p addedPats of
    Just idx -> idx : go addedPats rest
    Nothing -> (length addedPats) : go (addedPats ++ [p]) rest

compositePatterns' :: forall a. (Eq a) => [a] -> Int -> Int -> [[Int]]
compositePatterns' ls maxLength maxPatternNum = go ls []
 where
  go :: [a] -> [[a]] -> [[Int]]
  go [] _ = [[]]
  go rem foundPats = [patId : remCombinations | (patId, Just rest) <- zip [1 ..] $ stripPrefix rem <$> foundPats, remCombinations <- go rest foundPats] ++ newRes
   where
    newRes =
      if length foundPats >= maxPatternNum
        then []
        else
          [(length foundPats + 1) : remCombinations | n <- [1 .. min (length rem) maxLength], let (newPattern, rest) = splitAt n rem, remCombinations <- go rest (foundPats ++ [newPattern])]
