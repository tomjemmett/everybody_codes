module Events.Y2024.Quest01 where

import Common
import Data.List.Split (chunksOf)
import ECSolution (getInput)

quest01 :: String -> IO (Int, Int, Int)
quest01 = getInput 2024 1 (solve 1) (solve 2) (solve 3)

solve :: Int -> String -> Int
solve n = sum . map (scores . filter (/= 'x')) . chunksOf n

scores :: String -> Int
scores "" = 0
scores xs = sum s + l * pred l
  where
    s = map score xs
    l = length xs

score :: Char -> Int
score = \case
  'B' -> 1
  'C' -> 3
  'D' -> 5
  _ -> 0
