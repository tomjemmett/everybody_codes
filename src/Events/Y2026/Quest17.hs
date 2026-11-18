module Events.Y2026.Quest17 where

import Algorithm.Search
import Common
import Data.Set qualified as S
import ECSolution (getInput)
import Text.Parsec qualified as P
import Text.Parsec.String (Parser)

type Position = (Point2d, Direction, Direction)

quest17 :: String -> IO (Int, Int, Int)
quest17 = getInput 2026 17 part1 part2 part3

part1 :: String -> Int
part1 = findPath . parseInput
  where
    findPath (grid, end) = fst res
      where
        Just res = dijkstra next (const . const 1) (== end) (0, 0)
        next = filter (`S.member` grid) . point2dNeighbours

part2 :: String -> Int
part2 = solve (+)

part3 :: String -> Int
part3 = solve min

solve :: (Int -> Int -> Int) -> String -> Int
solve resFn input = resFn east south
  where
    (grid, end) = parseInput input
    east = run North East
    south = run West South
    ---
    run :: Direction -> Direction -> Int
    run edge dir =
      succ . length . takeWhile (not . done) $
        iterate move ((0, 0), edge, dir)
    ---
    done :: Position -> Bool
    done (p, edge, _) = p == end && edge `elem` [South, East]
    ---
    move :: Position -> Position
    move (p, edge, dir)
      | p' `S.notMember` grid = turn (p, edge, dir)
      | q' `S.member` grid = blocked (q', edge, dir)
      | otherwise = (p', edge, dir)
      where
        p' = moveOneStepInDir p dir
        q' = moveOneStepInDir p' edge

        turn :: Position -> Position
        turn (p, edge, dir) = (p, dir, flipDir edge)

        blocked :: Position -> Position
        blocked (p, edge, dir) = (p, flipDir dir, edge)

        flipDir :: Direction -> Direction
        flipDir = \case
          North -> South
          South -> North
          East -> West
          West -> East

parseInput :: String -> (S.Set Point2d, Point2d)
parseInput input = (S.fromList grid, end)
  where
    grid =
      [ (r, c)
      | (r, row) <- zip [0 ..] (lines input),
        (c, v) <- zip [0 ..] row,
        v == '#'
      ]
    end = maximum $ grid
