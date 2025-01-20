{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}

module N14 (getSolutions14) where

import Control.Arrow
import Control.Monad (forM_, void)
import Control.Monad.Trans.State
import Data.Char (isAlphaNum)
import Data.Either (fromRight)
import Data.Map ((!))
import qualified Data.Map as M
import Data.Tuple (swap)
import Data.Void (Void)
import Debugging
import GHC.IO (liftIO, unsafePerformIO)
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char (char, letterChar, newline, space, string)
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec.Debug (MonadParsecDbg (dbg))
import Useful (getSolutions)

type Resource = (String, Integer)
data Reaction = Reaction {productAmount :: Integer, reqs :: [Resource]} deriving (Show)
type ReactionMap = M.Map String Reaction
type SParser = Parsec Void String
resourseParser :: SParser Resource
resourseParser = fmap swap $ (,) <$> (space *> L.lexeme space L.decimal) <*> takeWhile1P Nothing isAlphaNum

reactionParser :: SParser (String, Reaction)
reactionParser = do
  reqs <- sepBy resourseParser (string ",")
  void $ string " =>"
  (productName, productAmount) <- resourseParser
  return (productName, Reaction{productAmount, reqs})

parseFile :: String -> ReactionMap
parseFile = fromRight M.empty . runParser (M.fromList <$> sepEndBy reactionParser newline) ""

useSupplies :: String -> Integer -> State (M.Map String Integer, M.Map String Integer) Integer
useSupplies target requestedAmount = do
  (availableResources, _) <- get
  let consumedAmount = min requestedAmount (availableResources ! target)
  modify (first (M.adjust (subtract consumedAmount) target))
  return $ requestedAmount - consumedAmount

binarySearch :: (Integral a) => (a -> Bool) -> a -> a -> a
binarySearch f x0 x1
  | f x0 || x0 + 1 == x1 = x0
  | otherwise =
      let m = (x0 + x1) `div` 2
       in if f m
            then binarySearch f x0 m
            else binarySearch f m x1

calcResources :: ReactionMap -> String -> Integer -> State (M.Map String Integer, M.Map String Integer) ()
calcResources reactionMap target requestedAmount = do
  let addNewProduce amount = M.adjust (+ amount) target
  if target == "ORE"
    then modify $ addNewProduce requestedAmount *** addNewProduce requestedAmount
    else do
      amountToProduce <- useSupplies target requestedAmount
      let Reaction{productAmount, reqs} = reactionMap ! target
          reps = (amountToProduce + 1) `div` productAmount
      forM_ reqs $ \(reactant, amount) -> do
        reactantAmountToProduce <- useSupplies reactant (amount * reps)
        calcResources reactionMap reactant reactantAmountToProduce
        modify (first (M.adjust (subtract reactantAmountToProduce) reactant))
      modify $ addNewProduce amountToProduce *** addNewProduce amountToProduce

requiredOre :: ReactionMap -> Integer -> Integer -- (M.Map String Integer, M.Map String Integer)
requiredOre reactionMap fuel =
  let
    resourceMap = M.insert "ORE" 0 $ M.map (const 0) reactionMap
    (_, producedResources) = execState (calcResources reactionMap "FUEL" fuel) (resourceMap, resourceMap)
   in
    producedResources ! "ORE"

solution1 :: ReactionMap -> Integer
solution1 = flip requiredOre 1

solution2 :: ReactionMap -> Integer
solution2 reactionMap =
  let
    condition fuel = requiredOre reactionMap fuel > 1_000_000_000_000
   in
    binarySearch condition 100 10_000_000

getSolutions14 = getSolutions parseFile solution1 solution2
