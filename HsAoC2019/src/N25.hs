module N25 (getSolutions25) where 
import IntCode
getSolutions25 :: String -> IO (String, Int)  
getSolutions25 filename = do 
  code <- codeParser <$> readFile filename 
  talkToMachine code []
  print "What password did Santa give you?"
  password <- getLine 
  return (password, 2019*50) 
  
