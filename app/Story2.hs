module Main where

import Stories.S2.Quests qualified as S2
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> mapM_ S2.runQuest [1 .. 3]
    (x : _) -> S2.runQuest (read x)
