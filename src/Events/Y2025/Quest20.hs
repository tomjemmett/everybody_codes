module Events.Y2025.Quest20 where

import Algorithm.Search
import Common
import Data.Bifunctor (first)
import Data.HashMap.Strict qualified as M
import Data.HashSet qualified as S
import Data.List (find)
import Data.Maybe (fromJust)
import ECSolution (getInput)

quest20 :: String -> IO (Int, Int, Int)
quest20 = getInput 2025 20 part1 part2 part3

part1 :: String -> Int
part1 input = flip div 2 $ length $ concatMap neighbors $ S.toList ts
  where
    (adjacents, _, _, ts) = parseInput input
    neighbors :: Point3d -> [Point3d]
    neighbors = filter (`S.member` ts) . init . adjacents

part2 :: String -> Int
part2 = fromJust . findPath 1 id

part3 :: String -> Int
part3 = fromJust . findPath 3 rotate120
  where
    rotate120 :: Point3d -> Point3d
    rotate120 (a, b, c) = (b, c, a)

findPath :: Int -> (Point3d -> Point3d) -> String -> Maybe Int
findPath rotations rotate input = fst <$> dijkstra neighbors cost goal (start, 0)
  where
    (adjacents, start, end, ts) = parseInput input
    neighbors :: (Point3d, Int) -> [(Point3d, Int)]
    neighbors (p, n) = map (,n') xs
      where
        xs = filter (`S.member` ts) $ map rotate $ adjacents p
        n' = succ n `mod` rotations
    cost :: (Point3d, Int) -> (Point3d, Int) -> Int
    cost = const . const 1
    goal :: (Point3d, Int) -> Bool
    goal = (== end) . fst

parseInput :: String -> (Point3d -> [Point3d], Point3d, Point3d, S.HashSet Point3d)
parseInput input = (adjacents, start, end, S.fromList $ map fst ts)
  where
    ts = map (first (getCoord size)) $ filter ((`elem` "EST") . snd) xs
    xs =
      concat
        [ [ ((x, y), v)
          | (x, v) <- zip [0 ..] ln
          ]
        | (y, ln) <- zip [0 ..] $ lines input
        ]
    size = maximum $ map (snd . fst) xs
    Just (start, _) = find ((== 'S') . snd) ts
    Just (end, _) = find ((== 'E') . snd) ts
    adjacents :: Point3d -> [Point3d]
    adjacents (a, b, c) =
      if even (a + b + c + size + 1)
        then [(a + 1, b, c), (a, b + 1, c), (a, b, c + 1), (a, b, c)]
        else [(a, b - 1, c), (a - 1, b, c), (a, b, c - 1), (a, b, c)]

-- create a transformed coordinate system:
-- from each corner, start labelling diagonally starting from n decreasing to 0
-- this gives us points like (8, 0, 0), then (8, 0, 1), (8, 0, 0), (8, 1, 0) etc.
-- these points are unique, but crucially allow us to easily
--   a. find adjacent points
--   b. rotate points around the triangle center
getCoord :: Int -> Point2d -> Point3d
getCoord n (x, y) = (n - x', y' `div` 2, y)
  where
    x' = (x + y + 1) `div` 2
    y' = x - y
