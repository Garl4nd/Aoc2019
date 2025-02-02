module ModularArithmetics (ModInteger (..), modInverse) where

import Data.Data (Proxy (Proxy))
import GHC.TypeLits

newtype (KnownNat n) => ModInteger n = ModI Integer deriving (Show, Eq)
instance forall n. (KnownNat n) => Num (ModInteger n) where
  (ModI x) + (ModI y) = ModI $ (x + y) `mod` natVal (Proxy :: Proxy n)
  (ModI x) * (ModI y) = ModI $ (x * y) `mod` natVal (Proxy :: Proxy n)
  (ModI x) - (ModI y) = ModI $ (x - y) `mod` natVal (Proxy :: Proxy n)
  negate (ModI x) = ModI $ negate x
  abs (ModI x) = ModI $ abs x
  signum (ModI x) = if x == 0 then 0 else 1
  fromInteger x = ModI $ x `mod` natVal (Proxy :: Proxy n)

modInverse :: (KnownNat n) => ModInteger n -> ModInteger n
modInverse modX@(ModI x) =
  let
    n = natVal modX
    (_, inv, _) = extendedEuclid x n
   in
    fromInteger inv

extendedEuclid :: Integer -> Integer -> (Integer, Integer, Integer)
extendedEuclid a b = go a b 1 0 0 1
 where
  go r0 0 s0 _ t0 _ = (r0, s0, t0)
  go r0 r1 s0 s1 t0 t1 =
    let (q, r) = r0 `quotRem` r1
        s = s0 - q * s1
        t = t0 - q * t1
     in go r1 r s1 s t1 t
