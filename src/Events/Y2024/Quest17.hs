module Events.Y2024.Quest17 where

import Common
import Data.Function (on)
import Data.HashMap.Strict qualified as M
import Data.HashSet qualified as S
import Data.List (minimumBy, sortBy)
import ECSolution (getInput)

quest17 :: String -> IO (Int, Int, Int)
quest17 = getInput 2024 17 p1 p2 p3
  where
    p1 = part1 . parseStars
    p2 = part2 . parseStars
    p3 = part3 . parseStars

parseStars :: String -> [Point2d]
parseStars input =
  [ (i, j)
  | (i, line) <- zip [0 ..] $ lines input,
    (j, v) <- zip [0 ..] line,
    v == '*'
  ]

prim :: Int -> [Point2d] -> [Int]
prim _ [] = []
prim maxD xs = (S.size v + r) : prim maxD xs'
  where
    (v, r) = go (S.singleton $ head xs, 0)
    xs' = filter (not . (`S.member` v)) xs
    edges :: M.HashMap Point2d [(Point2d, Int)]
    edges =
      M.map (filter ((< maxD) . snd)) $
        M.fromList
          [ (i, [(j, manhattanDistance i j) | j <- xs, i /= j])
          | i <- xs
          ]
    go :: (S.HashSet Point2d, Int) -> (S.HashSet Point2d, Int)
    go (visited, s)
      | null candidateEdges = (visited, s)
      | otherwise = go (S.insert j visited, d + s)
      where
        candidateEdges = concatMap (filter (not . (`S.member` visited) . fst) . (edges M.!)) $ S.toList visited
        (j, d) = minimumBy (compare `on` snd) candidateEdges

part1 :: [Point2d] -> Int
part1 = head . prim 1000

part2 :: [Point2d] -> Int
part2 = part1

part3 :: [Point2d] -> Int
part3 = product . take 3 . sortBy (flip compare) . prim 6
