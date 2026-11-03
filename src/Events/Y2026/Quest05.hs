module Events.Y2026.Quest05 where

import Common
import Control.Monad (forM_, guard, when)
import Control.Monad.State
import Data.HashMap.Strict qualified as M
import Data.List (nub)
import ECSolution (getInput)
import Text.Parsec qualified as P
import Text.Parsec.String (Parser)

quest05 :: String -> IO (Int, Int, Int)
quest05 = getInput 2026 5 part1 part2 part3

solve :: (M.HashMap Point2d Char -> M.HashMap Point2d Char) -> Char -> String -> Int
solve f c = M.size . M.filter (== c) . f . parseInput

part1 :: String -> Int
part1 = solve getLand '#'

part2 :: String -> Int
part2 = solve getSand '#'

part3 :: String -> Int
part3 = solve getTrees '@'

parseInput :: String -> M.HashMap Point2d Char
parseInput input = M.fromList do
  (row, line) <- zip [0 ..] $ lines input
  (col, v) <- zip [0 ..] line
  pure ((row, col), v)

getLand :: M.HashMap Point2d Char -> M.HashMap Point2d Char
getLand grid = execState (go $ point2dNeighboursDiags start) grid'
  where
    (maxRow, maxCol) = maximum $ M.keys grid
    start = (maxRow `div` 2, maxCol `div` 2)
    grid' = M.insert start '#' grid

getSand :: M.HashMap Point2d Char -> M.HashMap Point2d Char
getSand grid = execState (go starts) initialSand
  where
    land = getLand grid
    gridLand = M.filter (== '#') land
    points = M.keys gridLand
    initialSand = M.fromList do
      p <- points
      let n = point2dNeighboursDiags p
      let isSand = not $ all (`M.member` gridLand) n
      pure (p, if isSand then '#' else '0')
    starts = do
      p <- points
      let n = point2dNeighboursDiags p
      guard $ any (\q -> M.lookupDefault '.' q initialSand == '0') n
      pure p

getTrees :: M.HashMap Point2d Char -> M.HashMap Point2d Char
getTrees grid = states !! 2
  where
    states = iterate (execState growTrees) start
    sand = getSand grid
    points = M.keys sand
    initialTrees = M.fromList do
      p <- points
      guard $
        all (\q -> M.lookupDefault '#' q sand /= '#') $
          nub
            [ j
            | i <- point2dNeighboursDiags p,
              j <- point2dNeighbours i,
              j /= p
            ]
      pure (p, '@')
    start = M.union initialTrees sand
    growTrees :: State (M.HashMap Point2d Char) ()
    growTrees = do
      g <- get
      let candidates = M.keys $ M.filter (== '0') g
      forM_ candidates $ \p -> do
        let n = point2dNeighboursDiags p
        let vt = countTrue (\q -> g M.!? q == Just '@') n
        let vs = countTrue (\q -> g M.!? q == Just '#') n
        when (vt >= 4 && vs == 0) $ modify $ M.insert p '@'

go :: [Point2d] -> State (M.HashMap Point2d Char) ()
go [] = pure ()
go (p : ps) = do
  g <- get
  if g M.! p /= '#'
    then do
      ps' <- do
        let n = filter (`M.member` g) $ point2dNeighboursDiags p
        let v = countTrue (\q -> M.lookupDefault '0' q g /= '0') n
        if v >= 5
          then do
            modify $ M.insert p '#'
            pure $ filter (\q -> g M.! q /= '#') n
          else
            pure []
      go (ps ++ ps')
    else
      go ps
