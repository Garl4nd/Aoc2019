module N8 (getSolutions8) where

import qualified Data.Array as A
import Data.Char (ord)
import Data.Foldable (minimumBy)
import Data.Function (on)
import Data.List (find, intercalate)
import Useful

type Image = CharGrid

width, height :: Int
width = 25
height = 6
chunksOf :: Int -> [a] -> [[a]]
chunksOf n ls = case splitAt n ls of
  ([], _) -> []
  (chunk, rest) -> chunk : chunksOf n rest

parseFile :: String -> [Image]
parseFile file =
  let content = head . lines $ file
      layerStrs = chunksOf (width * height) content
      layerArs = A.listArray ((0, 0), (height - 1, width - 1)) <$> layerStrs
   in layerArs

solution1 :: [Image] -> Int
solution1 layers =
  let
    minLayer = minimumBy (compare `on` count '0') $ A.elems <$> layers
   in
    count '1' minLayer * count '2' minLayer

solution2 :: [Image] -> String
solution2 layers =
  let
    firstLayer = head layers
    topVisibleLayer = A.listArray (A.bounds firstLayer) [head [el | layer <- layers, let el = layer A.! idx, el /= '2'] | idx <- A.indices firstLayer]
    topVisibleLayerStr = charGridToStr topVisibleLayer
    widerLayer = map (concatMap (\s -> "  " <> (if s == '1' then "O" else " ") <> "  ")) topVisibleLayerStr
   in
    intercalate "\n" widerLayer

getSolutions8 = getSolutions parseFile solution1 solution2
