module N21 (getSolutions21) where

import IntCode
import Useful

-- part 1:  not 3 or not 1 and 4
springCode1 =
  unlines
    [ "NOT C T"
    , "NOT A J"
    , "OR T J"
    , "AND D J"
    , "WALK"
    ]
springCode2 =
  unlines
    [ "NOT C T"
    , "NOT F J"
    , "OR H J"
    , "AND J T"
    , "NOT A J"
    , "OR T J"
    , "OR D T"
    , "OR F T"
    , "NOT T T"
    , "OR T J"
    , "NOT B T"
    , "OR T J"
    , "AND D J"
    , "RUN"
    ]
getSolutions21 :: String -> IO (Int, Int)
getSolutions21 inputFile = do
  code <- codeParser <$> readFile inputFile
  talkToMachine code springCode1
  talkToMachine code springCode2
  print "What were the answers?"
  sol1 <- read <$> getLine
  sol2 <- read <$> getLine
  return (sol1, sol2)
