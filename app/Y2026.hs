module Main where

import Events.Y2026.Quests qualified as Y2026
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  if null args
    then mapM_ Y2026.runQuest [1 .. 20]
    else Y2026.runQuest (read $ head args)
