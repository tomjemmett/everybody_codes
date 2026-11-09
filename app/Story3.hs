module Main where

import Stories.S3.Quests qualified as S3
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> mapM_ S3.runQuest [1 .. 3]
    (x : _) -> S3.runQuest (read x)
