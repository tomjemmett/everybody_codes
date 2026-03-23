module Events.Y2024.Quest20 where

import Algorithm.Search
import Common
import Control.Monad (guard)
import Data.Function (on)
import Data.HashMap.Strict qualified as M
import Data.List (foldl', minimumBy, sort)
import Data.Maybe (maybe)
import Data.Sequence (Seq ((:<|), (:|>)), (><))
import Data.Sequence qualified as Seq
import ECSolution (getInput)

type Grid = M.HashMap Point2d Char

type Input = (Grid, Point2d)

quest20 :: String -> IO (Int, Int, Int)
quest20 = getInput 2024 20 p1 p2 p3
  where
    p1 = part1 . parseInput
    p2 = part2 . parseInput
    p3 = part3 . parseInput

parseInput :: String -> (Grid, Point2d)
parseInput input = (M.fromList g, start)
  where
    g =
      [ ((i, j), v)
        | (i, line) <- zip [0 ..] $ lines input,
          (j, v) <- zip [0 ..] line,
          v /= '#'
      ]
    start = fst . head $ filter ((== 'S') . snd) g

turnLeft :: Direction -> Direction
turnLeft = \case
  North -> West
  West -> South
  South -> East
  East -> North

turnRight :: Direction -> Direction
turnRight = \case
  North -> East
  East -> South
  South -> West
  West -> North

part1 :: Input -> Int
part1 (g, s) = bfs (Seq.singleton ((s, South), 1000, 0)) M.empty
  where
    bfs Seq.Empty m = succ . maximum $ M.elems m
    bfs (((p, d), h, t) :<| ps) m
      | t > 100 = bfs ps m
      | maybe False (>= h') (M.lookup (p, d) m) = bfs ps m
      | otherwise = bfs ps' m'
      where
        h' = case g M.! p of
          '+' -> h + 1
          '-' -> h - 2
          _ -> h - 1
        neighbors = do
          turn <- [id, turnLeft, turnRight]
          let d' = turn d
          let p' = moveOneStepInDir p d'
          guard (M.member p' g)
          pure ((p', d'), h', succ t)
        ps' = ps >< Seq.fromList neighbors
        m' = M.insert (p, d) h' m

part2 :: Input -> Int
part2 (g, s) = go 0 $ M.singleton (s, South, 0 :: Int) 10000
  where
    go t states
      | t > 0 && any isGoal (M.toList states) = t
      | M.null states = error "unreachable"
      | otherwise = go (succ t) $ M.foldlWithKey' (advance t) M.empty states
      where
        isGoal ((p, _, phase), h) = p == s && phase == 3 && h >= 10000
        advance _ acc _ h | h <= 0 = acc
        advance depth acc (p, d, phase) h
          | p == s && depth > 0 = acc
          | cell `elem` "ABC" && cell /= nextTarget phase = acc
          | h' <= 0 = acc
          | otherwise = foldl' step acc [id, turnLeft, turnRight]
          where
            cell = g M.! p
            h' = case cell of
              '+' -> h + 1
              '-' -> h - 2
              _ -> h - 1
            phase' = if cell == nextTarget phase then succ phase else phase
            step acc' turn
              | M.member p' g = M.insertWith max (p', d', phase') h' acc'
              | otherwise = acc'
              where
                d' = turn d
                p' = moveOneStepInDir p d'

        nextTarget = \case
          0 -> 'A'
          1 -> 'B'
          2 -> 'C'
          _ -> '_'

part3 :: Input -> Int
part3 (g, s) = go total (cycle x) 0
  where
    width = maximum $ map snd $ M.keys g
    height = succ $ maximum $ map fst $ M.keys g
    deltas = [sum $ f . (,j) <$> [0 .. height - 1] | j <- [1 .. width]]
      where
        f p = case M.lookup p g of
          Just c -> cellType c
          Nothing -> -1000
    cellType = \case
      '+' -> 1
      '-' -> -2
      '.' -> -1
      'S' -> -1
    delta = maximum deltas
    i = minimumBy (compare `on` \x -> abs (x - snd s)) $ map snd $ filter fst $ zip (map (delta ==) deltas) [1 ..]
    total = 384400 - abs (i - snd s) + 1
    x = map (cellType . snd) $ sort $ M.toList $ M.filterWithKey (\(_, k) _ -> k == i) g
    go 0 _ t = pred t
    go h (x : xs) t = go (h + x) xs (t + 1)
