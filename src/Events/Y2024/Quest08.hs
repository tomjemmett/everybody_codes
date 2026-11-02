module Events.Y2024.Quest08 where

import Common
import ECSolution (getInput)
import Text.Parsec qualified as P
import Text.Parsec.String (Parser)

quest08 :: String -> IO (Int, Int, Int)
quest08 = getInput 2024 8 p1 p2 p3
  where
    p1 = part1 <$> parse number
    p2 = p23 part2
    p3 = p23 part3
    p23 fn = parse do
      p <- number
      a <- number
      fn p a <$> number

part1 :: Int -> Int
part1 x = (v ^ 2 - x) * (2 * v - 1)
  where
    v = ceiling $ sqrt $ fromIntegral x

part2 :: Int -> Int -> Int -> Int
part2 p a b = go [1] 3 1
  where
    go hs n l =
      if b > r
        then go hs' (n + 2) t
        else (r - b) * n
      where
        t = p * l `mod` a
        hs' = t : t : map (+ t) hs
        r = sum hs'

part3 :: Int -> Int -> Int -> Int
part3 p a b = go [1] 3 1
  where
    go hs n l =
      if b > r
        then go hs' (n + 2) t
        else r - b
      where
        t = (p * l `mod` a) + a
        hs' = t : t : map (+ t) hs
        r = 2 * t + sum (map (\h -> h - p * n * h `mod` a) $ drop 2 hs')
