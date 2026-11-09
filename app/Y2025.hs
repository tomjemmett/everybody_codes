module Main where

import Events.Y2025.Quests qualified as Y2025
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> mapM_ Y2025.runQuest [1 .. 3]
    (x : _) -> Y2025.runQuest (read x)
