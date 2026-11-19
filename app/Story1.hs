module Main where

import Stories.S1.Quests qualified as Q
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> mapM_ Q.runQuest [1 .. 3]
    (x : _) -> Q.runQuest (read x)
