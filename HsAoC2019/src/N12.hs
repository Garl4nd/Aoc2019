{-# LANGUAGE NamedFieldPuns #-}

module N12 (getSolutions12) where

import Control.Applicative (liftA3)
import Data.Either (fromRight)
import Data.Maybe (fromJust)
import Data.Void (Void)
import Text.Megaparsec (Parsec, endBy, runParser, sepBy, sepEndBy)
import Text.Megaparsec.Char (char, letterChar, newline, space, string)
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec.Debug (MonadParsecDbg (dbg))
import Useful (cycleDetectionBrent, cycleDetectionFloyd', getSolutions)

type SParser = Parsec Void String

type PhaseCoords = ([Int], [Int])
data Moons = Moons
  { xs :: PhaseCoords
  , ys :: PhaseCoords
  , zs :: PhaseCoords
  }
  deriving (Show)

lexeme = L.lexeme space
integer = lexeme L.decimal
signedInt = L.signed space integer

moonParser :: SParser Moons
moonParser = aggregate <$> sepEndBy moonParser newline
 where
  moonParser :: SParser (Int, Int, Int)
  moonParser = liftA3 (,,) (string "<x=" *> signedInt) (string ", y=" *> signedInt) (string ", z=" *> signedInt <* string ">")
  aggregate :: [(Int, Int, Int)] -> Moons
  aggregate ls =
    let
      zeroVs = replicate (length ls) 0
      xs = ([x | (x, _, _) <- ls], zeroVs)
      ys = ([y | (_, y, _) <- ls], zeroVs)
      zs = ([z | (_, _, z) <- ls], zeroVs)
     in
      Moons{xs, ys, zs}

updateCoords :: PhaseCoords -> PhaseCoords
updateCoords (pos, vs) =
  let
    newVs = [v + sum [deltaV p p' | p' <- pos] | (p, v) <- zip pos vs]
    newPos = zipWith (+) pos newVs
    deltaV p1 p2
      | p1 < p2 = 1
      | p1 == p2 = 0
      | p1 > p2 = -1
   in
    (newPos, newVs)

totalEnergy :: Moons -> Int
totalEnergy Moons{xs = (x, vx), ys = (y, vy), zs = (z, vz)} = sum $ zipWith (*) kinEnergies potEnergies
 where
  energyFunc = zipWith3 (\qx qy qz -> abs qx + abs qy + abs qz)
  kinEnergies = energyFunc x y z
  potEnergies = energyFunc vx vy vz

solution1 :: Moons -> Int
solution1 moons = totalEnergy $ iterate (\Moons{xs, ys, zs} -> Moons{xs = updateCoords xs, ys = updateCoords ys, zs = updateCoords zs}) moons !! 1000

solution2 :: Moons -> Int
solution2 Moons{xs, ys, zs} =
  let
    (_, lamx) = cycleDetectionBrent updateCoords xs
    (_, lamy) = cycleDetectionBrent updateCoords ys
    (_, lamz) = cycleDetectionBrent updateCoords zs
   in
    lcm lamx $ lcm lamy lamz

parseFile :: String -> Moons
parseFile = fromJust . either (const Nothing) Just . runParser moonParser ""

getSolutions12 = getSolutions parseFile solution1 solution2
