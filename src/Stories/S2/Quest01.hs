module Stories.S2.Quest01 where

import Common
import Data.HashMap.Strict qualified as M
import Data.List (delete)
import ECSolution (getInput)
import Text.Parsec qualified as P
import Text.Parsec.String (Parser)

quest01 :: String -> IO (Int, Int, String)
quest01 = getInput 2 1 part1 part2 part3

part1 :: String -> Int
part1 input = sum r
  where
    (b, moves) = parseInput input
    r = zipWith (toss b) moves [0, 2 ..]

part2 :: String -> Int
part2 input = sum r
  where
    (b, moves) = parseInput input
    r = map (findBest b) moves

part3 :: String -> String
part3 input = show (minimum rs) ++ " " ++ show (maximum rs)
  where
    (b, moves) = parseInput input
    starts = [0, 2 .. fst (snd b)]
    possible_scores = map (\m -> map (toss b m) starts) moves
    xs = permutationsOfSize (length moves) [0 .. pred $ length starts]
    rs = map (sum . zipWith (!!) possible_scores) xs

parseInput :: String -> ((M.HashMap Point2d Char, Point2d), [String])
parseInput = parse do
  board <- pBoard
  moves <- pMoves
  let boardBounds = maximum $ map fst board
  pure ((M.fromList board, boardBounds), moves)
  where
    pBoard :: Parser [(Point2d, Char)]
    pBoard = do
      line <- P.many (P.oneOf "*.") `P.sepBy` P.newline
      pure $ concat $ zipWith f [1 ..] (map (zip [0 ..]) line)
    pMoves = P.many (P.oneOf "LR") `P.sepBy` P.newline
    f y xs = [((x, y), v) | (x, v) <- xs]

findBest :: (M.HashMap Point2d Char, Point2d) -> String -> Int
findBest b@(_, (maxX, _)) moves = maximum [toss b moves x | x <- [0, 2 .. maxX]]

score :: Int -> Int -> Int
score x y = max 0 $ f y * 2 - f x
  where
    f v = v `div` 2 + 1

toss :: (M.HashMap Point2d Char, Point2d) -> String -> Int -> Int
toss (board, (maxX, maxY)) moves ix = score ix $ go moves (ix, 0)
  where
    go [] (x, _) = x
    go moves (x, y)
      | x < 0 = go moves (1, y)
      | x > maxX = go moves (maxX - 1, y)
      | y > maxY = x
    go (m : ms) (x, y) = case board M.!? (x, y') of
      Just '.' -> go (m : ms) (x, y')
      Just '*' -> case m of
        'L' -> go ms (x - 1, y')
        'R' -> go ms (x + 1, y')
      Nothing -> x
      where
        y' = y + 1

permutationsOfSize :: Int -> [Int] -> [[Int]]
permutationsOfSize = go
  where
    go 0 _ = [[]]
    go _ [] = []
    go k ys =
      [ y : rest
      | y <- ys,
        rest <- go (k - 1) (delete y ys)
      ]
