module Main (
  main,
) where

import Control.Concurrent (getNumCapabilities)
import Control.Monad (unless, when, (>=>))
import Data.List (intercalate, isPrefixOf)
import InputDownloader (runFetchProblemDataToFiles)
import IntCode (codeParser, talkToMachine, runProgramIO)
import Lib
import System.Directory (copyFile)
import System.TimeIt (timeItNamed)
import Text.Read (readMaybe)
import Useful (trimChar, trimSpace, splitOn)

mainLoop :: IO ()
mainLoop = do
  threads <- getNumCapabilities
  putStrLn $ "Threads = " <> show threads
  putStrLn "Which problem do you want to solve?"
  prompt <- getLine
  if "talk" `isPrefixOf` prompt
    then do
      case splitOn ' ' prompt of 
        [_, codeFile] ->  (`talkToMachine` []). codeParser =<< readFile codeFile 
        [_, codeFile, inputFile] -> do 
          code <- codeParser <$> readFile codeFile 
          input <- readFile inputFile 
          talkToMachine code input 
	_ -> print "Wrong input"
      mainLoop
    else
      if "intc" `isPrefixOf` prompt
        then do
          code <- codeParser <$> (readFile . trimChar '"' . trimSpace . drop 4 $ prompt)
          runProgramIO code
        else unless (prompt `elem` ["e", "end"]) $ do
          let problemId = readMaybe prompt
          case problemId of
            Just day -> do
              let inputFile = "inputs/" <> show day <> ".txt"
              runFetchProblemDataToFiles 2019 day inputFile ("descriptions/" <> show day <> ".html")
              copyFile inputFile $ "../RsAoc2019/input/" <> show day <> ".txt"
              case maybeSolver day of
                Just solver -> do
                  (res1, res2) <- solver inputFile
                  putStrLn $ "The solution of problem #" <> show day <> " is: "
                  timeItNamed "Part 1 took " $ putStrLn ("Part1:\n" <> res1)
                  timeItNamed "Part 2 took " $ putStrLn ("Part2:\n" <> res2)
                Nothing -> putStrLn "Not yet solved"
            Nothing -> putStrLn "Not a number" --       return True
          mainLoop

main :: IO ()
main = do
  mainLoop
  putStrLn "Goodbye!"
