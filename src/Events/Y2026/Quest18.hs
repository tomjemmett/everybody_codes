module Events.Y2026.Quest18 where

import Common
import Data.List (sortOn)
import Data.Map.Strict qualified as M
import Data.Maybe (listToMaybe)
import ECSolution (getInput)
import Text.Parsec qualified as P
import Text.Parsec.String (Parser)

quest18 :: String -> IO (Int, Int, Int)
quest18 = getInput 2026 18 solve solve solve

solve :: String -> Int
solve = fill . parseInput

parseInput :: String -> [(Int, (Int, Int))]
parseInput = parse (p `P.sepEndBy` P.endOfLine)
  where
    p = do
      x <- P.string "x=" *> number
      y <- P.string "y=" *> number'
      w <- P.option 1 (P.string " width=" *> number')
      pure (x, (y, w))

fill :: [(Int, (Int, Int))] -> Int
fill = go (maxBound, minBound) 0 . sortOn (negate . fst . snd)
  where
    go _ acc [] = acc
    go (l, r) acc ((x, (h, w)) : rest) =
      go (l', r') (acc + (h - h') * (r' - l' + 1)) rest
      where
        h' = maybe 0 (fst . snd) (listToMaybe rest)
        (l', r') = (min l x, max r (x + w - 1))
