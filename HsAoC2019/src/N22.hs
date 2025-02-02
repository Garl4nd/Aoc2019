{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}

module N22 (getSolutions22) where

import Data.Bits
import Data.Data (Proxy (Proxy))
import Data.Either (fromRight)
import Data.Foldable (foldl')
import Data.Function.Memoize
import qualified Data.Set as S
import Data.Void (Void)
import Debug.Trace (traceWith)
import Debugging (traceWInfo)
import GHC.TypeLits (KnownNat, Nat, natVal)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Useful

type SParser = Parsec Void String
type Card = Integer
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

newStackSingle :: Integer -> Card -> Card
newStackSingle deckSize cardPos = deckSize - cardPos - 1

cutSingle :: Integer -> Integer -> Card -> Card
cutSingle deckSize n cardPos = (cardPos - n) `mod` deckSize

incSingle :: Integer -> Integer -> Card -> Card
incSingle deckSize n cardPos = n * cardPos `mod` deckSize

-- newStackSingleInv :: Integer -> Card -> Card
-- newStackSingleInv = newStackSingle

-- cutSingleInv :: Integer -> Integer -> Card -> Card
-- cutSingleInv deckSize n cardPos = (cardPos + n) `mod` deckSize

-- incSingleInv :: Integer -> Integer -> Card -> Card
-- incSingleInv deckSize n cardPos = (cardPos * modInverse n deckSize) `mod` deckSize

modInverse :: Integer -> Integer -> Integer
modInverse x n = let (_, k, _) = extendedEuclid x n in k `mod` n

extendedEuclid :: Integer -> Integer -> (Integer, Integer, Integer)
extendedEuclid a b = go a b 1 0 0 1
 where
  go r0 0 s0 _ t0 _ = (r0, s0, t0)
  go r0 r1 s0 s1 t0 t1 =
    let (q, r) = r0 `quotRem` r1
        s = s0 - q * s1
        t = t0 - q * t1
     in go r1 r s1 s t1 t

-- shuffleCards :: [ShuffleType] -> [Card] -> [Card]
-- shuffleCards shuffles cards = foldl' (flip shuffleOp) cards shuffles
--  where
--   shuffleOp NewStack = newStack
--   shuffleOp (Cut n) = cut n
--   shuffleOp (Inc n) = incDeal n

shuffleSingle :: [ShuffleType] -> Integer -> Card -> Card
shuffleSingle shuffles deckSize cardPos = foldl' (flip shuffleOp) cardPos shuffles
 where
  shuffleOp NewStack = newStackSingle deckSize
  shuffleOp (Cut n) = cutSingle deckSize n
  shuffleOp (Inc n) = incSingle deckSize n

checkInvs :: Integer -> (Integer -> Integer -> Integer) -> (Integer -> Integer -> Integer) -> [(Integer, Integer, Integer)]
checkInvs deckSize f fInv = [(n, r, nI) | n <- [0 .. deckSize - 1], let r = f deckSize n, let nI = fInv deckSize r, n /= nI]

-- shuffleSingleInv :: [ShuffleType] -> Integer -> Card -> Card
-- shuffleSingleInv shuffles deckSize cardPos = foldr shuffleOp cardPos shuffles
--  where
--   shuffleOp :: ShuffleType -> Card -> Card
--   shuffleOp NewStack = newStackSingleInv deckSize
--   shuffleOp (Cut n) = cutSingleInv deckSize n
--   shuffleOp (Inc n) = incSingleInv deckSize n

solution1 :: [ShuffleType] -> Integer
solution1 shuffles = shuffleSingle shuffles 10007 2019

type LinCoeffs = (Integer, Integer)

-- combineCoefs :: LinCoeffs -> LinCoeffs -> Integer -> LinCoeffs
-- combineCoefs (a, b) (a', b') modN =  ((a*a') `mod` modN, (a*b' + b) `mod` modN)

data (KnownNat n) => ModLinCoeffs n = ModLinCoeffs {a :: Integer, b :: Integer} deriving (Eq, Show)

instance (KnownNat n) => Semigroup (ModLinCoeffs n) where
  c@(ModLinCoeffs a1 b1) <> (ModLinCoeffs a2 b2) =
    let n = natVal c
     in ModLinCoeffs{a = (a1 * a2) `mod` n, b = (a1 * b2 + b1) `mod` n}

instance (KnownNat n) => Monoid (ModLinCoeffs n) where
  mempty = ModLinCoeffs 1 0

getLinFunc :: (KnownNat n) => ModLinCoeffs n -> (Integer -> Integer)
getLinFunc c@ModLinCoeffs{a, b} = f
 where
  f x = (a * x + b) `mod` n
  n = natVal c

inverseLinCoeffs :: (KnownNat n) => ModLinCoeffs n -> ModLinCoeffs n
inverseLinCoeffs c@(ModLinCoeffs a b) = ModLinCoeffs alpha beta
 where
  (alpha, beta) = (modInverse a n, (-(alpha * b)) `mod` n)
  n = natVal c

fastMonoidIter :: (Monoid f) => f -> Integer -> f
fastMonoidIter f n = go n f mempty
 where
  go k fPow accumFunc
    | k == 0 = accumFunc
    | otherwise = go (shiftR k 1) newFPow (if k .&. 1 == 1 then fPow <> accumFunc else accumFunc)
   where
    newFPow = fPow <> fPow

-- iterLinearCoeffs :: LinCoeffs -> Integer -> Integer -> LinCoeffs
-- iterLinearCoeffs f n modN = go 1 f (1,0) where
--   go k fPow accumIter
--     | k > n = accumIter
--     | otherwise = go (2*k) newFPow (if k .&. n /= 0 then combineCoefs fPow accumIter modN else accumIter) where
--       newFPow = combineCoefs fPow fPow modN

fastLinearIterate :: (KnownNat n) => ModLinCoeffs n -> Integer -> (Integer -> Integer)
fastLinearIterate coeffs iter = getLinFunc $ fastMonoidIter coeffs iter

solution2 :: [ShuffleType] -> Integer
solution2 shuffles =
  let
    shuffleCoeffs :: forall deckSizeT. (KnownNat deckSizeT) => ModLinCoeffs deckSizeT
    shuffleCoeffs = ModLinCoeffs a b
     where
      deckSize = natVal (Proxy :: Proxy deckSizeT)
      shufFunc = shuffleSingle shuffles deckSize
      (a, b) = ((shufFunc 1 - b) `mod` deckSize, shufFunc 0)
    inverseCoeffs = inverseLinCoeffs @119315717514047 shuffleCoeffs
   in
    fastLinearIterate inverseCoeffs 101741582076661 2020

getSolutions22 = getSolutions parseFile solution1 solution2
