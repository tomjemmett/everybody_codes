module Events.Y2026.Quest14 where

import Common
import Control.Monad (forM_)
import Control.Monad.State
import Data.HashMap.Strict qualified as M
import Data.HashSet qualified as S
import Data.List (minimumBy)
import Data.Maybe (catMaybes)
import Data.Ord (comparing)
import ECSolution (getInput)

type Node = Maybe Char

type Edge = (Node, Node, Int)

quest14 :: String -> IO (Int, Int, Int)
quest14 = getInput 2026 14 part1 part2 part3

part1 :: String -> Int
part1 = sum . map snd . filter (uncurry (>) . fst) . M.toList . parseInput

part2 :: String -> Int
part2 input = prim edges
  where
    edges = map (\((a, b), w) -> (a, b, w)) $ M.toList $ parseInput input

part3 :: String -> Int
part3 input = minimum $ map prim edges'
  where
    edges = map (\((a, b), w) -> (a, b, w)) $ M.toList $ parseInput input
    vertices = S.fromList $ map (\(_, b, _) -> b) $ edges
    edges' =
      [ map (alterEdge v) edges
      | v <- combinations2 $ catMaybes $ S.toList vertices
      ]
    alterEdge v (Just a, b, w) = (Just a, b, if a `elem` v then 0 else w)
    combinations2 :: [Char] -> [String]
    combinations2 [] = []
    combinations2 (x : xs) =
      [x : y : [] | y <- xs] ++ combinations2 xs

prim :: [Edge] -> Int
prim edges =
  go (S.singleton Nothing) (S.delete Nothing vertices) 0
  where
    vertices = S.fromList $ map (\(_, b, _) -> b) $ edges
    go connected remaining cost =
      if S.null remaining
        then cost
        else
          let (v, weight) = minimumBy (comparing snd) candidates
           in go
                (S.insert v connected)
                (S.delete v remaining)
                (cost + weight)
      where
        candidates =
          [ (v, w)
          | (a, b, w) <- edges,
            (u, v) <- [(a, b), (b, a)],
            u `S.member` connected,
            v `S.member` remaining
          ]

parseInput :: String -> M.HashMap (Node, Node) Int
parseInput input = execState (getEdges (M.keys i)) M.empty
  where
    i =
      M.fromList
        [ ((r, c), v)
        | (r, row) <- zip [0 ..] (lines input),
          (c, v) <- zip [0 ..] row
        ]
    getEdges [] = pure ()
    getEdges (p : ps) = do
      let v = i M.! p
      let ns = filter (/= Just v) $ map (`M.lookup` i) $ point2dNeighbours p
      forM_ ns $ \n -> modify $ M.insertWith (+) (Just v, n) 1
      getEdges ps