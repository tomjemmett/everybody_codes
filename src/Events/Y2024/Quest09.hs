module Events.Y2024.Quest09 where

import Common
import Data.Maybe (fromJust)
import Data.Vector qualified as V
import ECSolution (getInput)
import Text.Parsec qualified as P
import Text.Parsec.String (Parser)

quest09 :: String -> IO (Int, Int, Int)
quest09 = getInput 2024 9 p1 p2 p3
  where
    p1 = part1 <$> parse p
    p2 = part2 <$> parse p
    p3 = part3 <$> parse p
    p = number' `P.sepBy` P.newline

part1 :: [Int] -> Int
part1 xs = sum [dp V.! i | i <- xs]
  where
    stamps = [1, 3, 5, 10]
    dp = createDp (maximum xs) stamps

part2 :: [Int] -> Int
part2 xs = sum [dp V.! i | i <- xs]
  where
    stamps = [1, 3, 5, 10, 15, 16, 20, 24, 25, 30]
    dp = createDp (maximum xs) stamps

part3 :: [Int] -> Int
part3 xs = sum $ map go xs
  where
    go b =
      minimum $
        [ dp V.! (b1 - i) + dp V.! (b2 + i)
        | i <- [0 .. 50],
          b2 - b1 + 2 * i <= 100
        ]
      where
        b1 = b `div` 2
        b2 = b - b1
    stamps = [1, 3, 5, 10, 15, 16, 20, 24, 25, 30, 37, 38, 49, 50, 74, 75, 100, 101]
    dp = createDp (maximum xs) stamps

createDp :: Int -> [Int] -> V.Vector Int
createDp maxSparkball stamps = dp
  where
    dp = V.fromList [if x == 0 then 0 else minStamps x | x <- [0 .. maxSparkball]]
    minStamps x = minimum [dp V.! (x - stamp) + 1 | stamp <- stamps, x >= stamp]
