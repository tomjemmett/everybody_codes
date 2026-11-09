module Events.Y2026.Quest09 where

import Common
import Data.Map.Strict qualified as M
import ECSolution (getInput)
import Text.Parsec qualified as P
import Text.Parsec.String (Parser)

quest09 :: String -> IO (Integer, Integer, Integer)
quest09 = getInput 2026 9 part1 part2 part3

part1 :: String -> Integer
part1 = sum . map grow . parseInput
  where
    grow :: (Integer, Integer) -> Integer
    grow (growth, limit) = go 1 1 1 (M.singleton 1 1)
      where
        go :: Integer -> Integer -> Integer -> M.Map Integer Integer -> Integer
        go time size leaves branches
          | leaves >= limit = time
          | size + branchCount * timeSkip >= limit =
              time + (limit - size + branchCount - 1) `div` branchCount
          | otherwise = go time' size' leaves' branches'
          where
            timeSkip = growth - fst (M.findMax branches)
            branchCount = sum (M.elems branches)

            shifted = M.mapKeysMonotonic (+ timeSkip) branches
            growthCount = shifted M.! growth

            branches' =
              M.delete growth $
                M.insertWith (+) 1 growthCount $
                  M.insertWith (+) (growth `div` 2) (2 * growthCount) shifted

            size' = size + branchCount * timeSkip + growthCount
            leaves' = leaves + growthCount
            time' = time + timeSkip

part2 :: String -> Integer
part2 = solve

part3 :: String -> Integer
part3 = solve

solve :: String -> Integer
solve = sum . map grow . parseInput
  where
    grow :: (Integer, Integer) -> Integer
    grow (growth, limit) = go 1 1 (M.singleton 1 1)
      where
        go :: Integer -> Integer -> M.Map Integer Integer -> Integer
        go time n branches
          | n >= limit = time
          | otherwise = go time' n' branches'
          where
            timeSkip = growth - fst (M.findMax branches)

            shifted = M.mapKeysMonotonic (+ timeSkip) branches

            growthCount = shifted M.! growth

            branches' =
              M.delete growth $
                M.insertWith (+) 1 growthCount $
                  M.insertWith (+) (growth `div` 2) (2 * growthCount) shifted

            n' = n + growthCount
            time' = time + timeSkip

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
