{-# LANGUAGE FlexibleInstances #-}

module Lib (
  maybeSolver,
) where

import GraphUtils (Distance)
import N1
import N10
import N11
import N12
import N13
import N14
import N15
import N17
import N18
import N19
import N2
import N20
import N21
import N22
import N23
import N24
import N25
import N3
import N4
import N5
import N6
import N7
import N8
import N9

class SolutionResult a where
  showRes :: a -> String

instance SolutionResult Int where
  showRes = show

instance SolutionResult Integer where
  showRes = show

instance SolutionResult String where
  showRes = id
instance SolutionResult Distance where
  showRes = show
instance SolutionResult a => SolutionResult (Maybe a) where 
  showRes (Just res) =  showRes res 
  showRes Nothing = "Didn't find a solution!"

maybeSolver :: Int -> Maybe (String -> IO (String, String))
maybeSolver day = solFunc
 where
  solFunc = case day of
    1 -> stringize <$> Just getSolutions1
    2 -> stringize <$> Just getSolutions2
    3 -> stringize <$> Just getSolutions3
    4 -> stringize <$> Just getSolutions4
    5 -> stringize <$> Just getSolutions5
    6 -> stringize <$> Just getSolutions6
    7 -> stringize <$> Just getSolutions7
    8 -> stringize <$> Just getSolutions8 -- 9 -> Just $ getSolutions9 "inputs/9.txt"
    9 -> stringize <$> Just getSolutions9 -- 9 -> Just $ getSolutions9 "inputs/9.txt"
    10 -> stringize <$> Just getSolutions10
    11 -> stringize <$> Just getSolutions11
    12 -> stringize <$> Just getSolutions12
    13 -> stringize <$> Just getSolutions13
    14 -> stringize <$> Just getSolutions14
    15 -> stringize <$> Just getSolutions15
    -- 16 -> Just getSolutions16
    17 -> stringize <$> Just getSolutions17
    18 -> stringize <$> Just getSolutions18
    19 -> stringize <$> Just getSolutions19
    20 -> stringize <$> Just getSolutions20
    21 -> stringize <$> Just getSolutions21
    22 -> stringize <$> Just getSolutions22
    23 -> stringize <$> Just getSolutions23
    24 -> stringize <$> Just getSolutions24
    25 -> stringize <$> Just getSolutions25
    _ -> Nothing
  stringize :: (SolutionResult a, SolutionResult b) => (String -> IO (a, b)) -> (String -> IO (String, String))
  stringize solFunc input = do
    (sol1, sol2) <- solFunc input
    return (showRes sol1, showRes sol2)
