{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}

module N22 (getSolutions22) where

import Data.Bits
import Data.Data (Proxy (Proxy))
import Data.Either (fromRight)
import Data.Foldable (foldl')
import Data.Group
import Data.Void (Void)
import GHC.TypeLits (KnownNat, Nat, natVal)
import ModularArithmetics
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Useful

type SParser = Parsec Void String
data ShuffleType = NewStack | Inc Integer | Cut Integer deriving (Show)

numParser :: SParser Integer
numParser = L.signed spaceConsumer (lexeme L.decimal)
 where
  lexeme = L.lexeme spaceConsumer
  spaceConsumer = L.space hspace1 empty empty

shuffleParser :: SParser ShuffleType
shuffleParser =
  let incParser = string "deal with increment " *> (Inc <$> L.decimal)
      stackParser = NewStack <$ string "deal into new stack"
      cutParser = string "cut " *> (Cut <$> numParser)
   in incParser <|> stackParser <|> cutParser

parseFile :: String -> [ShuffleType]
parseFile file = fromRight [] $ runParser (sepEndBy shuffleParser newline) "" file

newStackSingle :: (KnownNat n) => ModInteger n -> ModInteger n
newStackSingle cardPos = negate cardPos - 1

cutSingle :: (KnownNat n) => ModInteger n -> ModInteger n -> ModInteger n
cutSingle = subtract

incSingle :: (KnownNat n) => ModInteger n -> ModInteger n -> ModInteger n
incSingle = (*)

shuffleSingle :: forall n. (KnownNat n) => [ShuffleType] -> ModInteger n -> ModInteger n
shuffleSingle shuffles cardPos = foldl' (flip shuffleOp) cardPos shuffles
 where
  shuffleOp :: ShuffleType -> ModInteger n -> ModInteger n
  shuffleOp NewStack = newStackSingle
  shuffleOp (Cut k) = cutSingle (fromInteger k)
  shuffleOp (Inc k) = incSingle (fromInteger k)

solution1 :: [ShuffleType] -> Integer
solution1 shuffles = let ModI res = shuffleSingle @10007 shuffles 2019 in res

data (Field k) => LinCoeffs k = LinCoeffs {a :: k, b :: k} deriving (Eq, Show)

linFunc :: (Field k) => LinCoeffs k -> (k -> k)
linFunc LinCoeffs{a, b} x = a * x + b

instance (Field k) => Semigroup (LinCoeffs k) where
  LinCoeffs a1 b1 <> LinCoeffs a2 b2 =
    LinCoeffs{a = a1 * a2, b = a1 * b2 + b1}

instance (Field k) => Monoid (LinCoeffs k) where
  mempty = LinCoeffs 1 0

instance (Field k) => Group (LinCoeffs k) where
  invert (LinCoeffs a b) = LinCoeffs alpha beta
   where
    (alpha, beta) = (multInverse a, -(alpha * b))

fastLinearIterate :: (Field k) => LinCoeffs k -> Integer -> (k -> k)
fastLinearIterate coeffs iter = linFunc $ fastMonoidIter coeffs iter -- fastMonoidIter  is already provided in base: == flip stimes/stimesMonoid

solution2 :: [ShuffleType] -> Integer
solution2 shuffles =
  let
    shuffleCoeffs :: forall deckSize. (KnownNat deckSize) => LinCoeffs (ModInteger deckSize)
    shuffleCoeffs = LinCoeffs a b
     where
      shufFunc = shuffleSingle shuffles
      (a, b) = (shufFunc 1 - b, shufFunc 0)
    inverseCoeffs = invert $ shuffleCoeffs @(119315717514047)
    ModI res = fastLinearIterate inverseCoeffs 101741582076661 2020
   in
    res

getSolutions22 = getSolutions parseFile solution1 solution2

-- newStack :: [Card] -> [Card]
-- newStack = reverse

-- cut :: Integer -> [Card] -> [Card]
-- cut n cards = let (topCut, bottomCut) = splitAt (fromInteger n `mod` length cards) cards in bottomCut ++ topCut

-- -- only works when n and the number of cards is coprime
-- incDeal :: Integer -> [Card] -> [Card]
-- incDeal n cards =
--   let
--     modInverseM = memoize (`modInverse` (toInteger $ length cards))
--     modInv = modInverseM n
--    in
--     [cards !! fromInteger (k * modInv `mod` (toInteger $ length cards)) | k <- [0 .. toInteger $ length cards - 1]]

-- newStackSingle :: Integer -> Card -> Card
-- newStackSingle deckSize cardPos = deckSize - cardPos - 1
--
-- cutSingle :: Integer -> Integer -> Card -> Card
-- cutSingle deckSize n cardPos = (cardPos - n) `mod` deckSize
--
-- incSingle :: Integer -> Integer -> Card -> Card
-- incSingle deckSize n cardPos = n * cardPos `mod` deckSize
--
-- newStackSingleInv :: Integer -> Card -> Card
-- newStackSingleInv = newStackSingle

-- cutSingleInv :: Integer -> Integer -> Card -> Card
-- cutSingleInv deckSize n cardPos = (cardPos + n) `mod` deckSize

-- incSingleInv :: Integer -> Integer -> Card -> Card
-- incSingleInv deckSize n cardPos = (cardPos * modInverse n deckSize) `mod` deckSize

-- modInverse :: Integer -> Integer -> Integer
-- modInverse x n = let (_, k, _) = extendedEuclid x n in k `mod` n
--
-- shuffleCards :: [ShuffleType] -> [Card] -> [Card]
-- shuffleCards shuffles cards = foldl' (flip shuffleOp) cards shuffles
--  where
--   shuffleOp NewStack = newStack
--   shuffleOp (Cut n) = cut n
--   shuffleOp (Inc n) = incDeal n

-- shuffleSingle :: [ShuffleType] -> Integer -> Card -> Card
-- shuffleSingle shuffles deckSize cardPos = foldl' (flip shuffleOp) cardPos shuffles
--  where
--   shuffleOp NewStack = newStackSingle deckSize
--   shuffleOp (Cut n) = cutSingle deckSize n
--   shuffleOp (Inc n) = incSingle deckSize n
--
-- shuffleSingleInv :: [ShuffleType] -> Integer -> Card -> Card
-- shuffleSingleInv shuffles deckSize cardPos = foldr shuffleOp cardPos shuffles
--  where
--   shuffleOp :: ShuffleType -> Card -> Card
--   shuffleOp NewStack = newStackSingleInv deckSize
--   shuffleOp (Cut n) = cutSingleInv deckSize n
--   shuffleOp (Inc n) = incSingleInv deckSize n

-- type LinCoeffs = (Integer, Integer)

-- combineCoefs :: LinCoeffs -> LinCoeffs -> Integer -> LinCoeffs
-- combineCoefs (a, b) (a', b') modN =  ((a*a') `mod` modN, (a*b' + b) `mod` modN)

-- iterLinearCoeffs :: LinCoeffs -> Integer -> Integer -> LinCoeffs
-- iterLinearCoeffs f n modN = go 1 f (1,0) where
--   go k fPow accumIter
--     | k > n = accumIter
--     | otherwise = go (2*k) newFPow (if k .&. n /= 0 then combineCoefs fPow accumIter modN else accumIter) where
--       newFPow = combineCoefs fPow fPow modN
