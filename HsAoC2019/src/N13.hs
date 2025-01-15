module N13 () where

import IntCode
import Useful

solution1 :: [Int] -> Int
solution1 code =
  let
    output = machineOutputs $ runCodeWInputST [] code
    objects = chunksOf 3 output
    tilePos = [(x, y) | [x, y, objId] <- objects, objId == 2]
   in
    length tilePos

-- renderGame  :: [Int] -> [String]
getSolutions13 = getSolutions codeParser solution1 (const 0)
