module Events.Y2026.Quest10 where

import Common
import Control.Monad (forM, when)
import Control.Monad.RWS.Strict
import Data.HashMap.Strict qualified as M
import Data.Monoid (Sum (Sum), getSum)
import ECSolution (getInput)

type Walls = M.HashMap Point2d Char

type Ghosts = M.HashMap Point2d [(Char, Integer)]

quest10 :: String -> IO (Integer, Integer, Integer)
quest10 = getInput 2026 10 part1 part2 part3

part1 :: String -> Integer
part1 i = getSum $ snd $ execRWS (runRounds 10) walls ghosts
  where
    (ghosts, walls) = parseInput i
    runRounds :: Integer -> RWS Walls (Sum Integer) Ghosts ()
    runRounds 0 = pure ()
    runRounds n = runRound (const True) >> runRounds (n - 1)

part2 :: String -> Integer
part2 i = fst $ evalRWS (runRounds (< limit) 0) walls ghosts
  where
    (ghosts, walls) = parseInput i
    limit = 10000

    runRounds :: (Integer -> Bool) -> Integer -> RWS Walls (Sum Integer) Ghosts Integer
    runRounds f n = do
      s <- get
      let v = length $ concat $ M.elems s
      if v == 0
        then pure n
        else do
          runRound f
          runRounds f (n + 1)

part3 :: String -> Integer
part3 = const 0

parseInput :: String -> (Ghosts, Walls)
parseInput input = (ghosts, walls)
  where
    points = do
      (row, line) <- zip [0 ..] $ lines input
      (col, char) <- zip [0 ..] line
      pure ((row, col), char)
    ghosts = M.map ((: []) . (,0)) $ M.fromList $ filter ((`elem` "<>^v") . snd) points
    walls = M.fromList $ filter ((`elem` "#") . snd) points

runRound :: (Integer -> Bool) -> RWS Walls (Sum Integer) Ghosts ()
runRound f = do
  s <- get
  let ghosts = concatMap (\(p, cs) -> map (p,) cs) $ M.toList s

  ghosts' <- M.fromListWith (++) <$> forM ghosts moveGhost
  let ghosts'' = forM ghosts' ghostCollision
  put . M.map (filter (f . snd)) =<< ghosts''

  pure ()

moveGhost :: (Point2d, (Char, Integer)) -> RWS Walls (Sum Integer) Ghosts (Point2d, [(Char, Integer)])
moveGhost (p@(row, col), (c, n)) = do
  let (p', c') =
        case c of
          '>' -> ((row, col + 1), 'v')
          '<' -> ((row, col - 1), '^')
          '^' -> ((row - 1, col), '>')
          'v' -> ((row + 1, col), '<')
  r <- wallCollision p'
  pure $ if r then (p, [(c', n + 1)]) else (p', [(c, n)])

wallCollision :: Point2d -> RWS Walls (Sum Integer) Ghosts Bool
wallCollision p = do
  walls <- ask
  let v = p `M.member` walls
  when v $ tell $ Sum 1
  pure v

ghostCollision :: [(Char, Integer)] -> RWS Walls (Sum Integer) Ghosts [(Char, Integer)]
ghostCollision [c] = pure [c]
ghostCollision cs = do
  tell $ Sum (fromIntegral $ length cs)
  pure $ map f cs
  where
    f (c, n) =
      ( case c of
          '>' -> '<'
          '<' -> '>'
          '^' -> 'v'
          'v' -> '^',
        n + 1
      )
