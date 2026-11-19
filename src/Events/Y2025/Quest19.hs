module Events.Y2025.Quest19 where

import Common hiding (Interval)
import Data.HashMap.Strict qualified as M
import Data.List (foldl', sortOn)
import ECSolution (getInput)
import Text.Parsec qualified as P

quest19 :: String -> IO (Int, Int, Int)
quest19 = getInput 2025 19 solve solve solve

data Interval = Interval
  { lower :: !Int,
    upper :: !Int
  }
  deriving (Show, Eq, Ord)

type Gaps = M.HashMap Int [Interval]

intersect ::
  Int ->
  Interval ->
  Int ->
  Interval ->
  Maybe Interval
intersect x1 (Interval y1Low y1High) x2 (Interval y2Low y2High)
  | x1 >= x2 = Nothing
  | yLow <= yHigh = Just (Interval yLow yHigh)
  | otherwise = Nothing
  where
    dx = x2 - x1

    y3Low = y1Low - dx
    y3High = y1High + dx

    yLow = max y2Low y3Low + (x2 + max y2Low y3Low) `mod` 2
    yHigh = min y2High y3High - (x2 + min y2High y3High) `mod` 2

mergeIntervals :: [Interval] -> [Interval]
mergeIntervals = foldr merge [] . sortOn lower
  where
    merge interval [] = [interval]
    merge (Interval lo hi) (Interval nextLo nextHi : rest)
      | nextLo <= hi =
          Interval lo (max hi nextHi) : rest
      | otherwise =
          Interval lo hi : Interval nextLo nextHi : rest

solve :: String -> Int
solve (parseInput -> gaps) =
  case foldl' step (0, [Interval 0 0]) (sortOn fst $ M.toList gaps) of
    (x, Interval y _ : _) ->
      y + (x - y) `div` 2
    _ ->
      error "No reachable intervals"
  where
    step (x, reachable) (xNext, nextGaps) =
      let reachableNext =
            [ interval
            | current <- reachable,
              gap <- nextGaps,
              Just interval <- [intersect x current xNext gap]
            ]
       in (xNext, mergeIntervals reachableNext)

parseInput :: String -> Gaps
parseInput = M.fromListWith (++) . parse (p `P.sepBy` P.newline)
  where
    p = do
      x <- number' <* P.char ','
      h <- number' <* P.char ','
      s <- number'
      pure (x, [Interval h (h + s)])