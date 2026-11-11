module Events.Y2026.Quest12 where

import Common
import Control.Applicative (liftA2)
import Control.Monad (forM_)
import Control.Monad.State.Strict
import Data.Bifunctor (first, second)
import Data.Function (on)
import Data.HashMap.Strict qualified as M
import Data.HashSet qualified as S
import Data.List (maximumBy)
import ECSolution (getInput)
import Text.Parsec qualified as P
import Text.Parsec.String (Parser)

quest12 :: String -> IO (Int, Int, Int)
quest12 = getInput 2026 12 part1 part2 part3

part1 :: String -> Int
part1 = sum . evalState go . parseInput

part2 :: String -> Int
part2 = sum . evalState go . parseInput

part3 :: String -> Int
part3 = sum . evalState go . parseInput

parseInput :: String -> S.HashSet Point2d
parseInput input =
  S.fromList
    [ (r, c)
    | (r, row) <- zip [0 ..] (lines input),
      (c, v) <- zip [0 ..] row,
      v == '#'
    ]

go :: State (S.HashSet Point2d) [Int]
go = do
  points <- get
  if S.null points
    then pure []
    else liftA2 (:) castSpell go

castSpell :: State (S.HashSet Point2d) Int
castSpell = do
  points <- get
  let p = head $ S.toList points
  ns <- findNeighbours [p] S.empty
  modify (`S.difference` ns)
  pure $ maximumMatching ns

findNeighbours :: [Point2d] -> S.HashSet Point2d -> State (S.HashSet Point2d) (S.HashSet Point2d)
findNeighbours [] collected = pure collected
findNeighbours (p@(x, y) : xs) collected =
  if p `S.member` collected
    then findNeighbours xs collected
    else do
      points <- get
      let collected' = S.insert p collected
      let ns = filter (`S.member` points) $ point2dNeighbours p
      findNeighbours (ns ++ xs) collected'

buildGraph :: S.HashSet Point2d -> M.HashMap Int [Int]
buildGraph points = go (S.toList points) M.empty
  where
    horizontal = assignRuns (second pred) (second succ)
    vertical = assignRuns (first pred) (first succ)
    go :: [Point2d] -> M.HashMap Int [Int] -> M.HashMap Int [Int]
    go [] g = g
    go (p : ps) g = go ps $ M.insertWith (++) h [v] g
      where
        h = horizontal M.! p
        v = vertical M.! p
    assignRuns ::
      (Point2d -> Point2d) ->
      (Point2d -> Point2d) ->
      M.HashMap Point2d Int
    assignRuns prev next = go (S.toList points) M.empty 0
      where
        go [] runIds _ = runIds
        go (p : ps) runIds runId
          | p `M.member` runIds = go ps runIds runId
          | prev p `S.member` points = go ps runIds runId
          | otherwise =
              let run = takeWhile (`S.member` points) $ iterate next p
                  runIds' = M.fromList $ map (,runId) run
               in go ps (M.union runIds runIds') (runId + 1)

maximumMatching :: S.HashSet Point2d -> Int
maximumMatching points = M.size matching
  where
    graph = buildGraph points
    matching = execState go M.empty

    go :: State (M.HashMap Int Int) ()
    go = forM_ (M.keys graph) $ \h ->
      augment h S.empty >> pure ()

    augment ::
      Int ->
      S.HashSet Int ->
      State (M.HashMap Int Int) (Bool, S.HashSet Int)
    augment h visited = try (graph M.! h) visited
      where
        try [] visited = pure (False, visited)
        try (v : vs) visited
          | v `S.member` visited = try vs visited
          | otherwise = do
              let visited' = S.insert v visited
              matching <- get

              case M.lookup v matching of
                Nothing -> do
                  modify (M.insert v h)
                  pure (True, visited')
                Just previous -> do
                  (r, visited'') <- augment previous visited'
                  if r
                    then do
                      modify (M.insert v h)
                      pure (True, visited'')
                    else try vs visited''