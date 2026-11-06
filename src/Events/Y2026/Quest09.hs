module Events.Y2026.Quest09 where

import Common
import Data.Map.Strict qualified as M
import ECSolution (getInput)
import Text.Parsec qualified as P
import Text.Parsec.String (Parser)

quest09 :: String -> IO (Integer, Integer, Integer)
quest09 = getInput 2026 9 part1 part2 part3

part1 :: String -> Integer
part1 = solve fst

part2 :: String -> Integer
part2 = solve snd

part3 :: String -> Integer
part3 = solve snd

solve :: ((Integer, Integer) -> Integer) -> String -> Integer
solve f = sum . map (grow f) . parseInput

parseInput :: String -> [(Integer, Integer)]
parseInput = parse (p `P.sepEndBy` P.endOfLine)
  where
    p :: Parser (Integer, Integer)
    p = do
      P.string "id=" *> number
      growth <- P.string ", growth=" *> number
      limit <- P.string ", limit=" *> number
      pure (growth, limit)
    number :: Parser Integer
    number = read <$> P.many1 P.digit

grow :: ((Integer, Integer) -> Integer) -> (Integer, Integer) -> Integer
grow f (growth, limit) = go 1 (1, 0, 1) (M.singleton 1 1)
  where
    go :: Integer -> (Integer, Integer, Integer) -> M.Map Integer Integer -> Integer
    go time (c, b, n) branches
      | f (c + b, n) >= limit = time
      | otherwise = go time' (c', b', n') branches'
      where
        timeSkip = growth - fst (M.findMax branches)

        shifted = M.mapKeysMonotonic (+ timeSkip) branches

        growthCount = shifted M.! growth

        branches' =
          M.delete growth $
            M.insertWith (+) 1 growthCount $
              M.insertWith (+) (growth `div` 2) (2 * growthCount) shifted

        c' = c + 2 * growthCount
        b' = b + growthCount
        n' = n + growthCount
        time' = time + timeSkip
