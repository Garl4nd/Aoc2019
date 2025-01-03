module Main (
  main,
) where

import Control.Concurrent (getNumCapabilities)
import Control.Monad (unless, (>=>))
import InputDownloader (runFetchProblemDataToFiles)
import Lib
import System.Directory (copyFile)
import System.TimeIt (timeItNamed)
import Text.Read (readMaybe)
import Useful (splitOn)

mainLoop :: IO ()
mainLoop = do
  threads <- getNumCapabilities
  putStrLn $ "Threads = " <> show threads
  putStrLn "Which problem do you want to solve?"
  prompt <- getLine
  unless (prompt `elem` ["e", "end"]) $ do
    let problemId = readMaybe prompt
    case problemId of
      Just day -> do
        let inputFile = "inputs/" <> show day <> ".txt"
        runFetchProblemDataToFiles 2019 day inputFile ("descriptions/" <> show day <> ".html")
        copyFile inputFile $ "../RsAoc2019/input/" <> show day <> ".txt"
        case maybeSolver day of
          Just solver -> do
            res <- solver inputFile
            timeItNamed "The solution took " $ putStrLn $ "The solution of problem #" <> show day <> " is: " <> show res
          Nothing -> putStrLn "Not yet solved"
      Nothing -> putStrLn "Not a number" --       return True
    mainLoop

main :: IO ()
main = do
  mainLoop
  putStrLn "Goodbye!"
