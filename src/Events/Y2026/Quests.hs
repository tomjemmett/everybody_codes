module Events.Y2026.Quests
  ( quest01,
    -- quest02,
    -- quest03,
    -- quest04,
    -- quest06,
    -- quest06,
    -- quest07,
    -- quest08,
    -- quest09,
    -- quest10,
    -- quest11,
    -- quest12,
    -- quest13,
    -- quest14,
    -- quest16,
    -- quest16,
    -- quest17,
    -- quest18,
    -- quest19,
    -- quest20,
    runQuest,
    downloadNotes,
  )
where

import Common
import Control.Monad (when)
import Data.Bifunctor (bimap)
import Data.Char (toUpper)
import ECSolution
import Events.Y2026.Quest01 (quest01)
-- import Events.Y2026.Quest02 (quest02)
-- import Events.Y2026.Quest03 (quest03)
-- import Events.Y2026.Quest04 (quest04)
-- import Events.Y2026.Quest05 (quest05)
-- import Events.Y2026.Quest06 (quest06)
-- import Events.Y2026.Quest07 (quest07)
-- import Events.Y2026.Quest08 (quest08)
-- import Events.Y2026.Quest09 (quest09)
-- import Events.Y2026.Quest10 (quest10)
-- import Events.Y2026.Quest11 (quest11)
-- import Events.Y2026.Quest12 (quest12)
-- import Events.Y2026.Quest13 (quest13)
-- import Events.Y2026.Quest14 (quest14)
-- import Events.Y2026.Quest15 (quest15)
-- import Events.Y2026.Quest16 (quest16)
-- import Events.Y2026.Quest17 (quest17)
-- import Events.Y2026.Quest18 (quest18)
-- import Events.Y2026.Quest19 (quest19)
-- import Events.Y2026.Quest20 (quest20)
import GetInputs (downloadNotes, submitAnswer)
import System.Directory (doesFileExist)
import System.TimeIt (timeIt)
import Text.Printf (printf)

quests =
  [ fmap makeSolution . quest01
    -- fmap makeSolution . quest02,
    -- fmap makeSolution . quest03,
    -- fmap makeSolution . quest04,
    -- fmap makeSolution . quest06,
    -- fmap makeSolution . quest06,
    -- fmap makeSolution . quest07,
    -- fmap makeSolution . quest08,
    -- fmap makeSolution . quest09,
    -- fmap makeSolution . quest10,
    -- fmap makeSolution . quest11,
    -- fmap makeSolution . quest12,
    -- fmap makeSolution . quest13,
    -- fmap makeSolution . quest14,
    -- fmap makeSolution . quest16,
    -- fmap makeSolution . quest16,
    -- fmap makeSolution . quest17,
    -- fmap makeSolution . quest18,
    -- fmap makeSolution . quest19,
    -- fmap makeSolution . quest20
  ]

runQuest :: Int -> IO ()
runQuest quest = do
  let d = quests !! pred quest
  contents <- readFile (createPath "sample" 2026 quest 1)

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
