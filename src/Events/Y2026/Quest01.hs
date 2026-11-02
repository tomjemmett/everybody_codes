module Events.Y2026.Quest01 where

import Common
import Data.List (splitAt)
import ECSolution (getInput)
import Text.Parsec qualified as P
import Text.Parsec.String (Parser)

quest01 :: String -> IO (Int, Int, Int)
quest01 = getInput 2026 1 part1 part2 part3

part1 :: String -> Int
part1 = solve (map (^ 2) [1..])

part2 :: String -> Int
part2 = solve (map (\x -> 2 * x + (2 * (x - 2))) [3..])

part3 :: String -> Int
part3 = solve (map (pred . (^ 2)) [3,5..])

parseInput :: String -> [Int]
parseInput = parse (number `P.sepBy` P.char ',')

solve :: [Int] -> String -> Int
solve xs = sum . map fn . parseInput
  where
    fn :: Int -> Int
    fn n = head (dropWhile (< n) xs) - n
