module Events.Y2024.Quest02 where

import Common
import Data.List (nub, sortBy, splitAt, transpose)
import ECSolution (getInput)
import Text.Parsec qualified as P
import Text.Parsec.String (Parser)

quest02 :: String -> IO (Int, Int, Int)
quest02 = getInput 2024 2 part1 part2 part3

parseInput :: String -> ([String], [String])
parseInput = parse p
  where
    p = do
      P.string "WORDS:"
      w <- commaSeparated (P.many P.letter)
      P.newline
      P.newline
      t <- P.many (P.noneOf "\n") `P.sepBy` P.newline
      pure (w, t)

part1 :: String -> Int
part1 (parseInput -> (words, target)) = p1 $ unwords target
  where
    p1 [] = 0
    p1 target = f words target
      where
        f [] (_ : xs) = p1 xs
        f (w : ws) xs@(_ : xs')
          | x == w = succ $ p1 xs'
          | otherwise = f ws xs
          where
            x = take (length w) xs

part2 :: String -> Int
part2 (parseInput -> (w, target)) = length . nub . concat $ p2 (unwords target) 0 []
  where
    words = sortDesc $ w ++ map reverse w
    p2 [] _ res = res
    p2 target i res = f words target
      where
        f [] (_ : xs) = p2 xs (succ i) res
        f (w : ws) xs@(_ : xs')
          | x == w = p2 xs' (succ i) res'
          | otherwise = f ws xs
          where
            lw = length w
            x = take lw xs
            res' = [i .. (i + lw - 1)] : res

part3 :: String -> Int
part3 (parseInput -> (words, target)) = length . nub . concat $ p3
  where
    p3 = do
      let target_h =
            -- convert all the targets into a list where each line of the targets
            -- is split into it's own list, with each character being represented as it's
            -- cartesian coordinate inside the grid, and the character
            -- replicate the horizontal line so we can handle "wrapping" from left to right
            map (concat . replicate 2) $
              [ [((i, j), t) | (j, t) <- zip [0 ..] ts]
              | (i, ts) <- zip [0 ..] target
              ]
          -- do the same for the vertical view of the grid, no need to handle "wrapping"
          target_v =
            [ [ ((i, j), t) | (i, t) <- zip [0 ..] ts
              ]
            | (j, ts) <- zip [0 ..] $ transpose target
            ]
          -- combine these into our new targets
          targets = target_h ++ target_v
      -- for each word
      w <- words
      -- for each target
      t <- targets
      -- find the length of the word and get the reverse version of the word
      let lw = length w
          m = reverse w
      -- for the current target line, get all of the substrings which are of the length of the word
      (i, j) <- fn t lw
      -- if the word is found in the target, return the found coordinates
      pure $ if (i == w) || (i == m) then j else []
    fn :: [(a, b)] -> Int -> [([b], [a])]
    fn [] _ = []
    fn t@(_ : ts) i = r : fn ts i
      where
        x = take i t
        r = (map snd x, map fst x)
