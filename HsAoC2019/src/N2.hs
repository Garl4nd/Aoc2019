module N2 (getSolutions2) where

import IntCode
import Useful (getSolutions)

runCodeWith :: Int -> Int -> Code -> Code
runCodeWith noun verb (start : _ : _ : rest) = finalCode $ runCodeWInputST [] (start : noun : verb : rest)
runCodeWith _ _ _ = []

solution1 :: Code -> Int
solution1 = head . runCodeWith 12 2

solution2 :: [Int] -> Int
solution2 code =
  let target = 19690720
   in head [100 * noun + verb | noun <- [0 .. 99], verb <- [0 .. 99], head (runCodeWith noun verb code) == target]

getSolutions2 = getSolutions codeParser solution1 solution2
