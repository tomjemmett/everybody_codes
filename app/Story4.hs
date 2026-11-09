module Main where

import Stories.S4.Quests qualified as S4
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> mapM_ S4.runQuest [1 .. 3]
    (x : _) -> S4.runQuest (read x)
