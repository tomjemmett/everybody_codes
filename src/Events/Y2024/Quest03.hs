module Events.Y2024.Quest03 where

import Common
import Control.Monad (guard)
import Data.HashSet qualified as H
import ECSolution (getInput)

type Grid = H.HashSet Point2d

quest03 :: String -> IO (Int, Int, Int)
quest03 = getInput 2024 3 part1 part2 part3

parseInput :: String -> Grid
parseInput input =
  H.fromList
    [ (i, j)
    | (i, xs) <- zip [0 ..] $ lines input,
      (j, x) <- zip [0 ..] xs,
      x == '#'
    ]

part1, part2, part3 :: String -> Int
part1 = solve point2dNeighbours . parseInput
part2 = part1
part3 = solve point2dNeighboursDiags . parseInput

solve :: (Point2d -> [Point2d]) -> Grid -> Int
solve _ grid | H.null grid = 0
solve fn grid = H.size grid + solve fn grid'
  where
    grid' = H.fromList do
      p <- H.toList grid
      guard $ all (`H.member` grid) $ fn p
      pure p
