module Stories.S3.Quest02 where

import Common
import Control.Monad (forM, forM_, when)
import Control.Monad.State
import Data.HashMap.Strict qualified as M
import Data.HashSet qualified as S
import Data.Sequence (Seq (..), (><))
import Data.Sequence qualified as Seq
import ECSolution (getInput)

quest02 :: String -> IO (Int, Int, Int)
quest02 = getInput 3 2 part1 part2 part3
  where
    part1 = solve True 1
    part2 = solve False 1
    part3 = solve False 3

parseInput :: String -> M.HashMap Point2d Char
parseInput i = M.fromList do
  (i, line) <- zip [0 ..] (lines i)
  (j, c) <- zip [0 ..] line
  pure ((i, j), c)

solve :: Bool -> Int -> String -> Int
solve isP1 movesRepeats input = evalState (go source moves 1) i'
  where
    i = parseInput input
    source = fst $ head $ M.toList $ M.filter (== '@') i
    targets = map fst $ M.toList $ M.filter (== '#') i

    i' = execState (forM_ targets fillPoints) i

    moves = cycle $ concatMap (replicate movesRepeats) [North, East, South, West]

    go :: Point2d -> [Direction] -> Int -> State (M.HashMap Point2d Char) Int
    go p (m : ms) t = do
      let p' = moveOneStepInDir p m
      gets (M.lookup p') >>= \case
        Just '.' -> do
          modify (M.insert p' '@' . M.insert p '+')
          if isP1 then pure () else fillPoints p'
          allM isSurrounded targets >>= \case
            True -> pure t
            False -> go p' ms (t + 1)
        Just c ->
          if isP1 && c == '#'
            then pure t
            else go p ms t
        _ -> growGrid p' *> go p (m : ms) t

isSurrounded :: Point2d -> State (M.HashMap Point2d Char) Bool
isSurrounded t = do
  g <- get
  let adjacent = map (moveOneStepInDir t) [North, East, South, West]
  pure $ all ((/= Just '.') . (`M.lookup` g)) adjacent

fillPoints :: Point2d -> State (M.HashMap Point2d Char) ()
fillPoints p = do
  forM_ [North, East, South, West] $ \dir -> do
    go (Seq.singleton $ moveOneStepInDir p dir) S.empty
  where
    go :: Seq Point2d -> S.HashSet Point2d -> State (M.HashMap Point2d Char) ()
    go Seq.Empty collected = modify $ \g -> foldr (`M.insert` '+') g collected
    go (p :<| ps) collected = do
      let dirs = [North, East, South, West]
      if p `S.member` collected
        then go ps collected
        else
          gets (M.lookup p) >>= \case
            Nothing -> pure ()
            Just '.' -> go (ps >< Seq.fromList (map (moveOneStepInDir p) dirs)) (S.insert p collected)
            _ -> go ps collected

growGrid :: Point2d -> State (M.HashMap Point2d Char) ()
growGrid (x, y) = do
  g <- get
  let k = map <$> [fst, snd] <*> [M.keys g]
  let [[minX, minY], [maxX, maxY]] = map <$> [minimum, maximum] <*> [k]
  when (x < minX || x > maxX) $ modify $ M.union $ M.fromList [((x, j), '.') | j <- [minY .. maxY]]
  when (y < minY || y > maxY) $ modify $ M.union $ M.fromList [((i, y), '.') | i <- [minX .. maxX]]
