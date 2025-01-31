{-# LANGUAGE TypeApplications #-}
module N17 (getSolutions17) where
import IntCode 
import Control.Arrow
import Data.Char
import Data.List (elemIndex, findIndex, find)
import Data.Maybe (isJust)
import Useful 
import Data.Array ((!))
import qualified Data.Array as A 
import Debugging (traceWInfo2, traceWInfo)
import Data.Array.IO.Internals (IOArray(IOArray))
stripPrefix :: (Eq a) => [a] -> [a] -> Maybe [a]
stripPrefix ls [] = Just ls
stripPrefix [] (p : _) = Nothing
stripPrefix (x : xs) (p : rest) = if x == p then stripPrefix xs rest else Nothing

listToPatterns :: (Eq a) => [[a]] -> [Char]
listToPatterns pats = (\i -> chr $ (ord 'A') + i) <$> go [] pats
 where
  go _ [] = []
  go addedPats (p : rest) = case elemIndex p addedPats of
    Just idx -> idx : go addedPats rest
    Nothing -> length addedPats : go (addedPats ++ [p]) rest

patternDecompositionToLetters :: (Eq a) => [a] -> Int -> Int -> [([[a]], String)]
patternDecompositionToLetters ls maxLength maxPatternNum = [(patterns, letterize <$> patSequence) |  (patterns, patSequence) <- patternDecomposition ls maxLength maxPatternNum]
  where letterize i = chr $ ord 'A' +i
patternDecomposition :: forall a. (Eq a) => [a] -> Int -> Int -> [([[a]], [Int])]
patternDecomposition ls maxLength maxPatternNum = go ls []
 where
  go :: [a] -> [[a]] -> [([[a]], [Int])]
  go [] foundPats = [(foundPats, [])]
  go rem foundPats = [(pattern, patId : remCombinations) | (patId, Just rest) <- zip [0 ..] $ stripPrefix rem <$> foundPats, (pattern, remCombinations) <- go rest foundPats] ++ newRes
   where
    newRes =
      if length foundPats >= maxPatternNum
        then []
        else
          [(pattern, (length foundPats) : remCombinations) | n <- [1 .. min (length rem) maxLength], let (newPattern, rest) = splitAt n rem, (pattern, remCombinations) <- go rest (foundPats ++ [newPattern])]


data Direction = N | S | W | E deriving (Show)
walk :: GridPos -> Direction -> GridPos
walk (y, x) N = (y - 1, x)
walk (y, x) S = (y + 1, x)
walk (y, x) W = (y, x - 1)
walk (y, x) E = (y, x + 1)

data Turn = L | R  deriving (Show, Eq) 
turn :: Direction -> Turn -> Direction 
turn N L = W 
turn N R = E 
turn W L = S
turn W R = N 
turn S L = E
turn S R = W 
turn E L = N 
turn E R = S 

getGridMap :: [Int] -> IO CharGrid 
getGridMap code = do 
  machine <- createMachine @IOArray code 
  iGrid <- getOutputs =<< runMachine [] machine 
  let gridStr = chr <$> iGrid
  return $ strToCharGrid $ unlines. init . lines  $ gridStr

trace :: Show a => String -> a -> a 
trace = traceWInfo False 
findPath :: CharGrid -> GridPos -> [(Turn, Int)]
findPath grid initPos =  go initPos N  where 
  go :: GridPos ->  Direction ->  [(Turn, Int)]
  go pos facing =  let
    possibleTurns = [(turnDir, turn facing turnDir ) | turnDir <- [L,R]]
    inBounds = A.inRange (A.bounds grid)
    straightWalk (newDir, newFacing) = trace "sw"  ((newDir, newFacing), takeWhile (\pos' -> trace "in bounds" (inBounds (trace "pos" pos')) && trace "correct val" (grid ! pos' == '#')) $ drop 1 $ iterate (`walk` newFacing) pos) 
    in case find (not. null . snd)  $ straightWalk <$> possibleTurns of 
      Nothing -> [] 
      Just ((turnDir, newFacing), straightWalk ) ->  (turnDir, length  straightWalk) : go (last straightWalk) newFacing 

intersectionScore :: CharGrid -> Int 
intersectionScore grid = sum . map (uncurry (*)) $ [(y-1, x-1) | pos@(y,x) <- A.indices grid, all (\pos' -> inBounds pos' && grid ! pos' == '#') (pos: neighbors4 pos)] where 
  inBounds = A.inRange (A.bounds grid)

getSolutions17 inputFile  = do 
  code <- codeParser <$> readFile inputFile 
  gridMap <- getGridMap code 
  let sol1 = intersectionScore gridMap 
      Just initialPos = find ( ('^'==).(gridMap !)) $ A.indices gridMap
      path = findPath gridMap initialPos 
      patterns = patternDecompositionToLetters path 10 3 
  print $ "sol1 = " <> show sol1
  print path
  print $ "Possible patterns = " <> show patterns 
  print  "Launching the conversation with the machine, good luck"
  _ <- getLine 
  talkToMachine (2: tail code ) []
  print  "Input the answer"
  sol2 <- getLine 
  return (sol1, sol2)
