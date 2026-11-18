module Events.Y2026.Quest16 where

import Common
import Data.Sequence (Seq (..), ViewL (..), ViewR (..), (<|), (><), (|>))
import Data.Sequence qualified as Seq
import ECSolution (getInput)

quest16 :: String -> IO (Int, Int, Int)
quest16 = getInput 2026 16 part1 part2 part3

part1 :: String -> Int
part1 = flipShade (const 1) . Seq.fromList

part2 :: String -> Int
part2 = flipShade id . Seq.fromList . concat . replicate 10

part3 :: String -> Int
part3 = flipShade id . Seq.fromList . concat . replicate 100

parseInput :: String -> Seq Char
parseInput = Seq.fromList

flipShade :: (Int -> Int) -> Seq Char -> Int
flipShade inc tablets = go (0, tablets)
  where
    go (!acc, Seq.Empty) = acc
    go (!acc, x :<| xs) = go $ goL acc x tail toFlip
      where
        (tail, toFlip) = Seq.spanr (/= x) $ Seq.dropWhileL (== x) xs

    goL !acc _ tail Seq.Empty = (acc, tail)
    goL !acc x tail xs = goR acc' x tail'' xs'
      where
        (tail', xs') = Seq.spanl (/= x) $ Seq.dropWhileR (== x) xs
        tail'' = Seq.reverse tail' >< tail
        acc' = acc + inc (Seq.length xs)

    goR !acc _ tail Seq.Empty = (acc, tail)
    goR !acc x tail xs = goL acc' x tail'' xs'
      where
        (tail', xs') = Seq.spanr (/= x) $ Seq.dropWhileL (== x) xs
        tail'' = tail' >< tail
        acc' = acc + inc (Seq.length xs)
