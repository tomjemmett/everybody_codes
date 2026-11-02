module Stories.S1.Quest01 where

import Common
import Control.Monad.State
import Data.HashMap.Strict qualified as M
import Data.List (splitAt)
import ECSolution (getInput)
import Math.NumberTheory.Moduli.Class (modulo, powSomeMod)
import Text.Parsec qualified as P

quest01 :: String -> IO (Int, Int, Int)
quest01 = getInput 1 1 part1 part2 part3

part1 :: String -> Int
part1 = parts12 pred

part2 :: String -> Int
part2 = parts12 (const 4)

parts12 :: (Int -> Int) -> String -> Int
parts12 f = maximum . map go . parseInput
  where
    go (xs, m) = sum $ map (eni f m) xs
    eni :: (Int -> Int) -> Int -> (Int, Int) -> Int
    eni rng mod (n, exp) =
      read $
        concat
          [ show $ powSomeMod nm (exp - i)
          | i <- [0 .. rng exp],
            exp - i >= 0
          ]
      where
        nm = toInteger n `modulo` fromIntegral mod

part3 :: String -> Int
part3 = maximum . map go . parseInput
  where
    go :: ([(Int, Int)], Int) -> Int
    go (xs, m) = sum $ map (eni m) xs
    eni m (n, exp) = getCycle [] M.empty 0 1
      where
        getCycle :: [Int] -> M.HashMap Int Int -> Int -> Int -> Int
        getCycle xs h i v = case h M.!? v' of
          Just i -> ans (reverse xs, i)
          Nothing -> getCycle (v' : xs) (M.insert v' i h) (succ i) v'
          where
            v' = (v * n) `mod` m
        ans :: ([Int], Int) -> Int
        ans (xs, i) = sum b * d + sum (take r b) + sum a
          where
            len = length xs
            exp' = exp - i
            (a, b) = splitAt i xs
            (d, r) = divMod exp' (length b)

parseInput :: String -> [([(Int, Int)], Int)]
parseInput = parse do
  P.many1 parseLine
  where
    parseLine = do
      a <- P.string "A=" *> number
      b <- P.string "B=" *> number
      c <- P.string "C=" *> number
      x <- P.string "X=" *> number
      y <- P.string "Y=" *> number
      z <- P.string "Z=" *> number
      m <- P.string "M=" *> number
      pure ([(a, x), (b, y), (c, z)], m)
