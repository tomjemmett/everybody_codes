module Events.Y2026.Quest19 where

import Common
import Data.HashMap.Strict qualified as M
import Data.List (group, permutations, sort, sortBy)
import ECSolution (getInput)
import GetInputs (downloadNotes)
import Text.Parsec qualified as P
import Text.Parsec.String (Parser)

quest19 :: String -> IO (Int, Int, Int)
quest19 = getInput 2026 19 part1 part2 part3

part1 :: String -> Int
part1 = getBest3 . combine . parseInput

part2 :: String -> Int
part2 input = maximum $ map getBest3 xs
  where
    (a, b) = parseInput input
    assignments = map (M.fromList . flip zip "MCYK") (permutations "1234")
    xs = map (\x -> combine (a, M.map (x M.!) b)) assignments

part3 :: String -> Int
part3 input
  | k >= a = k * a * (a + r)
  | r == 2 = a * (a + 1) * (a + 1)
  | r == 1 = a * a * (a + 1)
  | otherwise = a * a * a
  where
    i = M.fromListWith (+) $ map (,1) $ concat $ lines input
    k = i M.! 'K'
    s = sum (M.elems i) - k
    (a, r) = s `divMod` 3

parseInput :: String -> (M.HashMap Point2d Char, M.HashMap Point2d Char)
parseInput = parse p
  where
    p = do
      a <- q
      P.endOfLine
      b <- q
      pure (createGrid a, createGrid b)

    q = P.many1 (P.oneOf "CMYK1234") `P.sepEndBy` P.endOfLine

createGrid :: [String] -> M.HashMap Point2d Char
createGrid s =
  M.fromList
    [ ((r, c), v)
    | (r, row) <- zip [0 ..] s,
      (c, v) <- zip [0 ..] row
    ]

combine :: (M.HashMap Point2d Char, M.HashMap Point2d Char) -> M.HashMap Point2d Char
combine (a, b) = M.unionWith f a b
  where
    f 'K' _ = 'K'
    f _ 'K' = 'K'
    f 'C' 'M' = 'B'
    f 'C' 'Y' = 'G'
    f 'M' 'Y' = 'R'
    f x y
      | x == y = x
      | x > y = f y x
      | otherwise = error "Invalid combination"

getBest3 :: M.HashMap Point2d Char -> Int
getBest3 =
  product
    . take 3
    . sortBy (flip compare)
    . map length
    . group
    . sort
    . M.elems
