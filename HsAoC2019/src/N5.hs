module N5 (getSolutions5) where

import IntCode
import Useful

solution1 :: [Int] -> Int
solution1 = last . machineOutputs . runCodeWInputST [1]

solution2 :: [Int] -> Int
solution2 = last . machineOutputs . runCodeWInputST [5]

getSolutions5 = getSolutions codeParser solution1 solution2
