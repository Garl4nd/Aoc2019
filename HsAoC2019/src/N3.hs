module N3 () where

import Data.Either (fromRight)
import Data.Void (Void)
import Text.Megaparsec (Parsec, endBy, runParser, sepBy, sepEndBy)
import Text.Megaparsec.Char (char, letterChar, newline)
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec.Debug (MonadParsecDbg (dbg))
import Useful (GridPos, splitOn)

data Direction = U | L | R | D deriving (Eq, Show, Read)
data Instruction = Instruction Direction !Int deriving (Show)
type Wire = [Instruction]
type SParser = Parsec Void String
fileParser :: SParser [Wire]
fileParser = endBy wireParser newline
 where
  wireParser :: SParser [Instruction]
  wireParser = sepBy instParser (char ',')
  instParser = do
    direction <- read . replicate 1 <$> letterChar
    steps <- L.decimal
    return $ Instruction direction steps

parseFile :: String -> (Wire, Wire)
parseFile file =
  let
    [wire1, wire2] = fromRight [] $ runParser fileParser "" file
   in
    (wire1, wire2)

moveDir :: Instruction -> (GridPos -> GridPos)
moveDir (Instruction U steps) (y, x) = (y - steps, x)
moveDir (Instruction D steps) (y, x) = (y + steps, x)
moveDir (Instruction L steps) (y, x) = (y, x - steps)
moveDir (Instruction R steps) (y, x) = (y, x + steps)

pathSegments :: Wire -> [(GridPos, GridPos)]
pathSegments wire = let turns = scanl (flip moveDir) (0, 0) wire in zip turns (drop 1 turns)

intersections :: (Int, Int) -> (Int, Int) -> [Int]
intersections i1 i2 =
  let
    (as, bs) = minMaxPair i1
    (as', bs') = minMaxPair i2
   in
    if as < as'
      then
        if bs < as' then [] else [as' .. min bs bs']
      else
        if bs' < as' then [] else [as .. min bs bs']

minMaxPair (a, b) = (min a b, max a b)
between n a b = min a b <= n && n <= max a b
crossPoints :: (GridPos, GridPos) -> (GridPos, GridPos) -> [GridPos]
crossPoints ((y1s, x1s), (y1e, x1e)) ((y2s, x2s), (y2e, x2e))
  | y2s == y2e =
      if
        | y1s == y1e -> if y1s == y2s then [(y1s, x) | x <- intersections (x1s, x1e) (x2s, x2e)] else []
        | between y2s y1s y1e && between x1s x2s x2e -> [(y2s, x1s)]
        | otherwise -> []
  | otherwise =
      if
        | x1s == x1e -> if x1s == x2s then [(y, x1s) | y <- intersections (y1s, y1e) (y2s, y2e)] else []
        | between x2s x1s x1e && between y1s y2s y2e -> [(y1s, x2s)]
        | otherwise -> []

crossings :: (Wire, Wire) -> [GridPos]
crossings (wire1, wire2) = concat [crossPoints s1 s2 | s1 <- pathSegments wire1, s2 <- pathSegments wire2]

solution1 :: (Wire, Wire) -> Int
solution1 wires = minimum $ filter (> 0) $ manhattan <$> crossings wires
 where
  manhattan (y, x) = abs y + abs x

-- >>> parseFile <$> readFile "inputs/3.txt"
