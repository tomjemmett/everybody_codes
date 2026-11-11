module Stories.S4.Quest02 where

import Common
import Control.Monad.RWS
import Data.HashMap.Strict qualified as M
import Data.HashSet qualified as S
import Data.Sequence qualified as Q
import ECSolution (getInput)
import Text.Parsec qualified as P
import Text.Parsec.String (Parser)

quest02 :: String -> IO (Int, Int, Int)
quest02 = getInput 4 2 part1 part2 part3

part1 :: String -> Int
part1 = S.size . move . parseInput

part2 :: String -> Int
part2 = S.size . fireflies . move . parseInput

part3 :: String -> Int
part3 = S.size . fireflies . bfs . parseInput . (++ "\nMOVES=")

parseInput :: String -> (Point2d, M.HashMap Char Point2d, String)
parseInput = parse do
  (_, start) <- p
  xs <- P.manyTill p (P.string "MOVES=")
  moves <- P.many P.letter
  pure (start, M.fromList $ map (\([k], v) -> (k, v)) xs, moves)
  where
    p = do
      n <- P.many1 P.letter
      P.string "=["
      x <- number
      P.char ','
      y <- number
      P.char ']'
      P.endOfLine
      pure (n, (x, y))

move :: (Point2d, M.HashMap Char Point2d, String) -> S.HashSet Point2d
move (start, beacons, moves) = S.fromList $ scanl fn start moves
  where
    fn pos m = moveTowardsBeacon pos (beacons M.! m)

fireflies :: S.HashSet Point2d -> S.HashSet Point2d
fireflies s = S.difference ns s
  where
    ns = S.fromList $ concatMap point2dNeighbours s

moveTowardsBeacon :: Point2d -> Point2d -> Point2d
moveTowardsBeacon (px, py) (bx, by) = ((px + bx) `div` 2, (py + by) `div` 2)

bfs :: (Point2d, M.HashMap Char Point2d, String) -> S.HashSet Point2d
bfs (start, beacons, _) = go (Q.singleton start) (S.singleton start)
  where
    go queue seen =
      case Q.viewl queue of
        Q.EmptyL -> seen
        node Q.:< rest -> go' node rest
      where
        go' node rest = go queue' seen'
          where
            next = map (moveTowardsBeacon node) (M.elems beacons)
            unseen = filter (not . (`S.member` seen)) next
            seen' = S.union seen $ S.fromList unseen
            queue' = rest Q.>< Q.fromList unseen