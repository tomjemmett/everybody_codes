module Events.Y2026.Quest14 where

import Common
import Control.Monad (foldM, forM_, unless, when)
import Control.Monad.ST (ST, runST)
import Control.Monad.State
import Data.Array.ST
import Data.HashMap.Strict qualified as M
import Data.List (foldl', minimumBy)
import Data.Maybe (catMaybes)
import Data.Ord (comparing)
import Data.STRef
import Data.Set qualified as S
import Data.Vector qualified as V
import Data.Vector.Mutable qualified as MV
import ECSolution (getInput)

type Node = Maybe Char

type Edge = (Node, Node, Int)

type Graph = V.Vector [(Int, Int)]

quest14 :: String -> IO (Int, Int, Int)
quest14 = getInput 2026 14 part1 part2 part3

part1 :: String -> Int
part1 = sum . map snd . filter (uncurry (>) . fst) . M.toList . parseInput

part2 :: String -> Int
part2 = fst . prim . makeGraph . parseInput

part3 :: String -> Int
part3 input = minimum $ map (fst . prim) edges'
  where
    edges = parseInput input
    g = makeGraph edges
    edges' =
      [ makeGraph $ M.mapWithKey (alterEdge v) edges
      | v <- combinations2 $ catMaybes $ S.toList $ S.fromList $ map fst $ M.keys edges
      ]
    alterEdge v (Just a, b) w = if a `elem` v then 0 else w
    combinations2 :: [Char] -> [String]
    combinations2 [] = []
    combinations2 (x : xs) =
      [x : y : [] | y <- xs] ++ combinations2 xs

parseInput :: String -> M.HashMap (Maybe Char, Maybe Char) Int
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

makeGraph :: M.HashMap (Node, Node) Int -> Graph
makeGraph edges = graph
  where
    nodeMap :: M.HashMap Node Int
    nodeMap = M.fromList $ zipWith (,) (Nothing : (S.toList $ S.fromList $ map fst $ M.keys edges)) [0 ..]
    n = M.size nodeMap

    graph =
      V.accum
        (++)
        (V.replicate n [])
        ( concat
            [ [(u, [(v, w)]), (v, [(u, w)])]
            | ((a, b), w) <- M.toList edges,
              let u = nodeMap M.! a,
              let v = nodeMap M.! b
            ]
        )

data PrimState s = PrimState
  { visited :: STUArray s Int Bool,
    dist :: STUArray s Int Int,
    parent :: STUArray s Int Int,
    queue :: S.Set (Int, Int)
  }

relax :: Int -> (Int, Int) -> StateT (PrimState s) (ST s) ()
relax u (v, w) = do
  st <- get

  seen <- lift $ readArray (visited st) v
  old <- lift $ readArray (dist st) v

  unless (seen || w >= old) $ do
    lift $ writeArray (dist st) v w
    lift $ writeArray (parent st) v u

    modify' $ \s ->
      s {queue = S.insert (w, v) (queue s)}

go :: Graph -> Int -> [(Int, Int)] -> StateT (PrimState s) (ST s) (Int, [(Int, Int)])
go graph total edges = do
  st <- get

  case S.minView (queue st) of
    Nothing -> pure (total, reverse edges)
    Just ((w, u), rest) -> do
      modify' $ \s -> s {queue = rest}

      seen <- lift $ readArray (visited st) u

      if seen
        then go graph total edges
        else do
          lift $ writeArray (visited st) u True

          parentValue <- lift $ readArray (parent st) u

          let edges' =
                if u == 0
                  then edges
                  else (parentValue, u) : edges

          mapM_ (relax u) (graph V.! u)

          go graph (total + w) edges'

prim :: Graph -> (Int, [(Int, Int)])
prim graph = runST $ do
  let n = V.length graph

  visited <- newArray (0, n - 1) False
  dist <- newArray (0, n - 1) maxBound
  parent <- newArray (0, n - 1) (-1)

  writeArray dist 0 0

  let initialState =
        PrimState
          { visited = visited,
            dist = dist,
            parent = parent,
            queue = S.singleton (0, 0)
          }

  evalStateT (go graph 0 []) initialState