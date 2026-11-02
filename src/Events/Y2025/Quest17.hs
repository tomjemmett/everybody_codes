module Events.Y2025.Quest17 where

import Algorithm.Search
import Common
import Data.Char (digitToInt)
import Data.Function (on)
import Data.List (find, maximumBy)
import Data.Maybe (mapMaybe)
import Data.Vector qualified as V
import ECSolution (getInput)

quest17 :: String -> IO (Int, Int, Int)
quest17 = getInput 2025 17 part1 part2 part3

part1 :: String -> Int
part1 = costInRange 10 . parseInput

part2 :: String -> Int
part2 input = uncurry (*) $ maximumBy (compare `on` snd) $ zip [1 ..] vs'
  where
    i = parseInput input
    vs = map (`costInRange` i) [1 ..]
    vs' = takeWhile (> 0) $ zipWith (-) vs (0 : vs)

part3 :: String -> Int
part3 input = a * b
  where
    (volcano, grid) = parseInput input
    Just start = find ((== 0) . gridPointValue grid) $ allGridPoints grid
    i = (volcano, start, grid)
    ((a, b) : _) = mapMaybe go [1 ..]
    go radius =
      case getRecurliaLoop i radius of
        Nothing -> Nothing
        Just res' ->
          if res' < (radius + 1) * 30
            then Just (res', radius)
            else Nothing

getRecurliaLoop :: (Point2d, Point2d, V.Vector (V.Vector Int)) -> Int -> Maybe Int
getRecurliaLoop (volcano, start, grid) radius = fst <$> dijkstra neighbors cost goal (start, 0)
  where
    (vX, vY) = volcano
    cost :: (Point2d, Int) -> (Point2d, Int) -> Int
    cost _ ((x, y), _) = grid V.! y V.! x
    goal :: (Point2d, Int) -> Bool
    goal = ((start, 1) ==)
    inBounds :: Point2d -> Bool
    inBounds (x, y) =
      x >= 0
        && x < V.length (grid V.! 0)
        && y >= 0
        && y < V.length grid
        && not (inRadius radius volcano (x, y))
    neighbors :: (Point2d, Int) -> [(Point2d, Int)]
    neighbors ((x, y), z) = map updateZ ps
      where
        ps = filter inBounds $ point2dNeighbours (x, y)
        updateZ (nx, ny) = ((nx, ny), z')
          where
            z'
              | ny < vY = z
              | x < vX && nx >= vX = 1
              | nx < vX && x >= vX = 0
              | otherwise = z

inRadius :: Int -> Point2d -> Point2d -> Bool
inRadius r (vX, vY) (pX, pY) = (vX - pX) ^ 2 + (vY - pY) ^ 2 <= r ^ 2

gridPointValue :: V.Vector (V.Vector Int) -> Point2d -> Int
gridPointValue grid (x, y) = grid V.! y V.! x

allGridPoints :: V.Vector (V.Vector Int) -> [Point2d]
allGridPoints grid =
  [ (x, y)
  | x <- [0 .. V.length (grid V.! 0) - 1],
    y <- [0 .. V.length grid - 1]
  ]

costInRange :: Int -> (Point2d, V.Vector (V.Vector Int)) -> Int
costInRange radius (volcano, grid) = sum $ map (gridPointValue grid) fs
  where
    ps = allGridPoints grid
    fs = filter ((/= volcano) <&> inRadius radius volcano) ps

parseInput :: String -> (Point2d, V.Vector (V.Vector Int))
parseInput input = (volcano, grid)
  where
    ls = lines input
    grid = V.fromList $ map (V.fromList . map fn) ls
    volcano =
      head
        [ (x, y)
        | x <- [0 .. V.length (grid V.! 0) - 1],
          y <- [0 .. length grid - 1],
          grid V.! y V.! x == 10
        ]
    fn = \case
      '@' -> 10
      'S' -> 0
      c -> digitToInt c
