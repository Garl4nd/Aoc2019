module N9 (getSolutions9) where

import IntCode
import Useful (getSolutions)

getSolutions9 = getSolutions codeParser (last . machineOutputs . runCodeWInputST [1]) (last . machineOutputs . runCodeWInputST [2])
