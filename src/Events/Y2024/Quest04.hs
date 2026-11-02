module Events.Y2024.Quest04 where

import Common
import ECSolution (getInput)

quest04 :: String -> IO (Int, Int, Int)
quest04 = getInput 2024 4 p1 p2 p3
  where
    [p1, p2, p3] = (.) <$> [part1, part2, part3] <*> [parseInput]

parseInput :: String -> [Int]
parseInput = map read . lines

solve :: Int -> [Int] -> Int
solve m = sum . map (abs . subtract m)

part1 :: [Int] -> Int
part1 xs = solve (minimum xs) xs

part2 :: [Int] -> Int
part2 = part1

part3 :: [Int] -> Int
part3 xs = minimum $ map (`solve` xs) xs
