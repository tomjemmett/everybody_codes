module Main where

import Stories.S3.Quests qualified as Q
import System.Environment (getArgs)
import System.TimeIt (timeIt)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> timeIt $ do
      mapM_ Q.runQuest [1 .. 3]
      putStrLn $ replicate 80 '-'
      putStrLn "Completed all quests, total time:"
    (x : _) -> Q.runQuest (read x)
