module Events.Y2025.Quest08 where

import Common
import Data.HashMap.Strict qualified as M
import Data.List (inits)
import Data.Maybe (mapMaybe)
import ECSolution (getInput)

type Point = (Double, Double)

type Line = (Point, Point)

quest08 :: String -> IO (Int, Int, Int)
quest08 = getInput 2025 8 part1 part2 part3

part1 :: String -> Int
part1 input = countTrue (== d) $ map abs $ zipWith (-) (tail i) i
  where
    i = commaSeparatedInts input
    s = maximum i
    d = s `div` 2

part2 :: String -> Int
part2 input = sum $ zipWith checkIntersections (inits lines) lines
  where
    i = commaSeparatedInts input
    p = createCoordinates i
    lines = zipWith (createLine p) i (tail i)

part3 :: String -> Int
part3 input = max a b
  where
    i = commaSeparatedInts input
    p = createCoordinates i
    lines = zipWith (createLine p) i (tail i)
    otherLines =
      [ createLine p a b
      | a <- [1 .. maximum i],
        b <- [1 .. maximum i],
        a < b,
        (a, b) `notElem` zip i (tail i)
      ]
    a = succ $ maximum $ zipWith checkIntersections (inits lines) lines
    b = maximum $ map (checkIntersections lines) otherLines

createCoordinates :: [Int] -> M.HashMap Int Point
createCoordinates = M.fromList . zip [1 ..] . pointsOnCircle . maximum

pointsOnCircle :: Int -> [Point]
pointsOnCircle n =
  [ (cos theta, sin theta)
  | k <- [0 .. n - 1],
    let theta = 2 * pi * fromIntegral k / fromIntegral n
  ]

createLine :: M.HashMap Int Point -> Int -> Int -> Line
createLine p a b | a > b = createLine p b a
createLine p a b = (p M.! a, p M.! b)

intersection :: Line -> Line -> Maybe Point
intersection ((x1, y1), (x2, y2)) ((x3, y3), (x4, y4))
  | abs denom < eps = Nothing -- parallel or coincident
  | t > eps && t < 1 - eps && u > eps && u < 1 - eps = Just (x1 + t * dx1, y1 + t * dy1)
  | otherwise = Nothing
  where
    dx1 = x2 - x1
    dy1 = y2 - y1
    dx2 = x4 - x3
    dy2 = y4 - y3
    denom = dx1 * dy2 - dy1 * dx2
    dx3 = x3 - x1
    dy3 = y3 - y1
    t = (dx3 * dy2 - dy3 * dx2) / denom
    u = (dx3 * dy1 - dy3 * dx1) / denom
    eps = 1e-9

checkIntersections :: [Line] -> Line -> Int
checkIntersections lines line = length $ mapMaybe (intersection line) lines

i = commaSeparatedInts "1,5,2,6,8,4,1,7,3,5,7,8,2"

p = createCoordinates i

ls = zipWith (createLine p) i (tail i)
