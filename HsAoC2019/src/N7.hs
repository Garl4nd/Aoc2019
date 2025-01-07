module N7 () where 

import IntCode
import Data.List (foldl')

runAmplifiers :: [Int] -> [Int] -> Int
runAmplifiers code settings = foldl' (\input s -> head . snd $ runCodeWInputST code [s, input] ) 0 settings  


