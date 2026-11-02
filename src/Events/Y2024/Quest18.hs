module Events.Y2024.Quest18 where

import Common
import Data.Foldable (minimumBy)
import Data.Function (on)
import Data.HashMap.Strict qualified as M
import Data.HashSet qualified as S
import Data.Sequence (Seq ((:<|)), (><))
import Data.Sequence qualified as SQ
import ECSolution (getInput)

type Input = (S.HashSet Point2d, S.HashSet Point2d)

quest18 :: String -> IO (Int, Int, Int)
quest18 = getInput 2024 18 p1 p2 p3
  where
    p1 = part1 . parseInput
    p2 = part2 . parseInput
    p3 = part3 . parseInput

part1 :: Input -> Int
part1 (g, t) = maximum $ map (ds M.!) $ S.toList t
  where
    init = [minimum] <*> pure (S.toList g)
    ds = flood g init

part2 :: Input -> Int
part2 (g, t) = maximum $ map (ds M.!) $ S.toList t
  where
    init = [minimum, maximum] <*> pure (S.toList g)
    ds = flood g init

part3 :: Input -> Int
part3 (g, t) = snd $ minimumBy (compare `on` snd) $ M.toList ds'
  where
    ds = foldr1 (M.unionWith (+)) $ map (flood g . (: [])) $ S.toList t
    ds' = M.filterWithKey (\k _ -> not $ k `S.member` t) ds

parseInput :: String -> Input
parseInput input = (S.fromList $ map fst g, S.fromList trees)
  where
    g =
      [ ((i, j), v)
      | (i, line) <- zip [0 ..] $ lines input,
        (j, v) <- zip [0 ..] line,
        v /= '#'
      ]
    trees = [p | (p, v) <- g, v == 'P']

flood :: S.HashSet Point2d -> [Point2d] -> M.HashMap Point2d Int
flood g t = M.fromList $ go (SQ.fromList $ map (,0) t) S.empty
  where
    go :: SQ.Seq (Point2d, Int) -> S.HashSet Point2d -> [(Point2d, Int)]
    go xs _ | SQ.null xs = []
    go ((x, i) :<| xs) v = (x, i) : go xs' v'
      where
        ns = SQ.fromList $ map (,succ i) $ filter ((not . (`S.member` v)) <&> (`S.member` g)) $ point2dNeighbours x
        xs' = xs >< ns
        v' = S.insert x v
