module Events.Y2024.Quest19 where

import Common
import Control.Monad (forM_)
import Data.Array qualified as A
import ECSolution (getInput)
import Text.Parsec qualified as P
import Text.Parsec.String (Parser)

type Grid = A.Array Point2d Char

type Input = (String, Grid)

quest19 :: String -> IO (String, String, String)
quest19 = getInput 2024 19 p1 p2 p3
  where
    p1 = part1 . parseInput
    p2 = part2 . parseInput
    p3 = part3 . parseInput

parseInput :: String -> Input
parseInput = parse do
  instructions <- cycle <$> P.many1 (P.oneOf "LR") <* P.newline
  P.newline
  grid <- makeGrid <$> P.many P.anyChar
  pure (instructions, grid)

makeGrid :: String -> Grid
makeGrid input = A.array ((0, 0), b) x
  where
    x =
      [ ((i, j), v)
      | (i, line) <- zip [0 ..] $ lines input,
        (j, v) <- zip [0 ..] line
      ]
    b = maximum $ map fst x

runInstructions :: Input -> Input
runInstructions i@(i', _) = (i', snd $ foldl go i xs)
  where
    (_, (mi, mj)) = A.bounds $ snd i
    xs = [(i, j) | i <- [1 .. pred mi], j <- [1 .. pred mj]]
    go :: Input -> Point2d -> Input
    go (i : is, g) (x, y) = (is, g A.// xs)
      where
        n =
          cycle
            [ (x + x', y + y')
            | (x', y') <-
                [ (-1, -1),
                  (-1, 0),
                  (-1, 1),
                  (0, 1),
                  (1, 1),
                  (1, 0),
                  (1, -1),
                  (0, -1)
                ]
            ]
        m = take 8 $ drop (if i == 'R' then 1 else 7) n
        xs = zip m $ map (g A.!) n

part1 :: Input -> String
part1 = findAnswer . runInstructions

part2 :: Input -> String
part2 = findAnswer . (!! 100) . iterate runInstructions

part3 = const "-"

printGrid :: A.Array Point2d Char -> IO ()
printGrid g = do
  let (_, (mi, mj)) = A.bounds g
  forM_ [0 .. mi] \i -> do
    putStrLn [g A.! (i, j) | j <- [0 .. mj]]

findAnswer :: Input -> String
findAnswer (_, g) =
  if is /= ie
    then error "no answer"
    else [g A.! (is, j) | j <- [succ js .. pred je]]
  where
    find c = fst . head . filter ((== c) . snd) $ A.assocs g
    (is, js) = find '>'
    (ie, je) = find '<'
