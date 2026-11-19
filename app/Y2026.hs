module Main where

import Events.Y2026.Quests qualified as Q
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> mapM_ Q.runQuest [1 .. 20]
    (x : _) -> Q.runQuest (read x)
