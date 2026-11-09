module Stories.S4.Quests
  ( runQuest,
    downloadNotes,
    quest01,
    quest02,
    quest03,
  )
where

import Common
import Control.Monad (when)
import Data.Bifunctor (bimap)
import Data.Char (toUpper)
import ECSolution
import GetInputs (downloadNotes, submitAnswer)
import Stories.S4.Quest01 (quest01)
import Stories.S4.Quest02 (quest02)
import Stories.S4.Quest03 (quest03)
import System.Directory (doesFileExist)
import System.TimeIt (timeIt)
import Text.Printf (printf)

quests =
  [ fmap makeSolution . quest01,
    fmap makeSolution . quest02,
    fmap makeSolution . quest03
  ]

runQuest :: Int -> IO ()
runQuest quest = do
  let d = quests !! pred quest
  let path = createPath "sample" 2026 quest 1
  pathExists <- doesFileExist path
  when pathExists do
    contents <- readFile path

    when (contents /= "") $ timeIt do
      putStrLn $ replicate 80 '-'
      putStrLn $ "Quest: " ++ show quest
      putStrLn ""
      putStrLn "Sample:"
      d "sample" >>= showQuest
      putStrLn ""
      putStrLn "Actual:"
      d "actual" >>= showQuest
      putStrLn ""

tryAnswer :: Int -> Int -> IO ()
tryAnswer quest part = do
  Solution (s1, s2, s3) <- (quests !! pred quest) "sample"
  Solution (a1, a2, a3) <- (quests !! pred quest) "actual"
  let (sampleAnswer, actualAnswer) = bimap show show case part of
        1 -> (s1, a1)
        2 -> (s2, a2)
        3 -> (s3, a3)
  putStrLn $ printf "Sample answer for quest %d part %d: %s" quest part sampleAnswer
  putStrLn $ printf "Actual answer for quest %d part %d: %s" quest part actualAnswer
  putStr "Submit? "
  response <- getYNResonse
  when response do
    submitAnswer 2026 quest part actualAnswer
    when (part < 3) $ downloadNotes 2026 quest

getYNResonse :: IO Bool
getYNResonse = do
  putStr "Please answer y or n: "
  response <- getLine
  case map toUpper response of
    "Y" -> pure True
    "N" -> pure False
    _ -> getYNResonse
