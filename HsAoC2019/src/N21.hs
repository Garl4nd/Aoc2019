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

-- part 2: not 1 or ((not 2 or not 3) and 8) and 4. (not 2) or (not 3 and 8) would work as well.
springCode2 =
  unlines
    [ "NOT C T"
    , "NOT B J"
    , "OR T J "
    , "AND H J"
    , "NOT A T"
    , "OR T J "
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
