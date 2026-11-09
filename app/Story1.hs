module Main where

import Stories.S1.Quests qualified as S1
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> mapM_ S1.runQuest [1 .. 3]
    (x : _) -> S1.runQuest (read x)
