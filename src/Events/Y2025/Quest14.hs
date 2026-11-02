module Events.Y2025.Quest14 where

import Common
import Data.HashMap.Strict qualified as M
import Data.Vector qualified as V
import ECSolution (getInput)

quest14 :: String -> IO (Int, Int, Int)
quest14 = getInput 2025 14 part1 part2 part3

part1 :: String -> Int
part1 input = sum $ map activeCount $ take 10 $ tail rounds
  where
    tiles = parseInput input
    rounds = iterate tileRound tiles

part2 :: String -> Int
part2 input = sum $ map activeCount $ take 2025 $ tail rounds
  where
    tiles = parseInput input
    rounds = iterate tileRound tiles

part3 :: String -> Int
part3 input = d * sum targetMatchCounts + sum (take r targetMatchCounts)
  where
    target = parseInput input
    blank = unlines $ replicate 34 (replicate 34 '.')
    tiles = parseInput blank
    rounds = iterate tileRound tiles
    (start, end) = detectCycle M.empty rounds 0
    (d, r) = 1000000000 `divMod` (end - start)
    targetMatchCounts =
      map (\r -> if isTargetPattern target r then activeCount r else 0) $
        drop start $
          take end rounds

detectCycle :: M.HashMap String Int -> [V.Vector (V.Vector Char)] -> Int -> (Int, Int)
detectCycle seen (t : ts) step = case M.lookup k seen of
  Just firstSeen -> (firstSeen, step)
  Nothing -> detectCycle (M.insert k step seen) ts (step + 1)
  where
    k = tilesToString t

tileRound :: V.Vector (V.Vector Char) -> V.Vector (V.Vector Char)
tileRound tiles =
  V.fromList
    [ V.fromList [f (x, y) | y <- [0 .. ncol - 1]]
    | x <- [0 .. nrow - 1]
    ]
  where
    nrow = V.length tiles
    ncol = V.length (tiles V.! 0)
    diags (x, y) = filter inBounds [(x - 1, y - 1), (x - 1, y + 1), (x + 1, y - 1), (x + 1, y + 1)]
    inBounds (x, y) = x >= 0 && y >= 0 && x < nrow && y < ncol
    lookup (x, y) = tiles V.! x V.! y
    f p = case lookup p of
      '#' -> if odd neighbors then '#' else '.'
      '.' -> if even neighbors then '#' else '.'
      where
        neighbors = countTrue ((== '#') . lookup) $ diags p

activeCount :: V.Vector (V.Vector Char) -> Int
activeCount = sum . V.map (V.length . V.filter (== '#'))

isTargetPattern :: V.Vector (V.Vector Char) -> V.Vector (V.Vector Char) -> Bool
isTargetPattern v1 v2 =
  and
    [ v1 V.! x V.! y == v2 V.! (x + ad) V.! (y + ad)
    | x <- [0 .. s1 - 1],
      y <- [0 .. s1 - 1]
    ]
  where
    s1 = V.length v1
    s2 = V.length v2
    ad = (s2 - s1) `div` 2

parseInput :: String -> V.Vector (V.Vector Char)
parseInput input =
  V.fromList
    [ V.fromList i | i <- lines input
    ]

tilesToString :: V.Vector (V.Vector Char) -> String
tilesToString tiles = unlines [V.toList (tiles V.! x) | x <- [0 .. V.length tiles - 1]]
