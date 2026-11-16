{-# LANGUAGE TemplateHaskell #-}

module Events.Y2026.Quest15 where

import Algorithm.Search
import Common
import Control.Lens hiding (to)
import Data.Bifunctor (bimap, first, second)
import Data.HashMap.Strict qualified as M
import Data.List (foldl', sort)
import Data.Set qualified as S
import ECSolution (getInput)

data Graph = Graph
  { _grid :: M.HashMap Point2d [Edge],
    _start :: Point2d,
    _end :: Point2d
  }
  deriving (Show)

data Edge
  = Edge
  { _to :: !Point2d,
    _direction :: !Direction,
    _weight :: !Int,
    _sulferCount :: !Int,
    _sulferIx :: !Int
  }
  deriving (Show, Eq, Ord)

data Loop = Loop
  { loopWeight :: !Int,
    loopSulferCount :: !Int,
    loopEdges :: ![Edge]
  }
  deriving (Show)

makeLenses ''Graph
makeLenses ''Edge

quest15 :: String -> IO (Int, Int, Int)
quest15 = getInput 2026 15 part1 part2 part3

part1 :: String -> Int
part1 = findPathP1 . parseInput

part2 :: String -> Int
part2 input = f * w + p
  where
    graph = parseInput input
    bestLoop = findBestLoop graph
    target = 2026
    w = loopWeight bestLoop
    n = loopSulferCount bestLoop

    f = (target `div` n) - 3
    p = findPathP23 (>= target - f * n) graph

part3 :: String -> Int
part3 input = f * w + p
  where
    graph = parseInput input
    bestLoop = findBestLoop graph
    target = 202620262026
    w = loopWeight bestLoop
    n = loopSulferCount bestLoop

    f = (target `div` n) - 10
    p = findPathP23 (== target - f * n) graph

parseInput :: String -> Graph
parseInput input = compress g s e
  where
    g =
      M.fromList
        [ ((r, c), v)
        | (r, row) <- zip [0 ..] (lines input),
          (c, v) <- zip [0 ..] row,
          v /= '#'
        ]
    s = head $ M.keys $ M.filter (== 'S') g
    e = head $ M.keys $ M.filter (== 'E') g

compress :: M.HashMap Point2d Char -> Point2d -> Point2d -> Graph
compress g s e = Graph addStraightPaths s e
  where
    pointsBetween = map (fst s,) [succ $ snd s .. pred $ snd e]
    row = fst s

    loops = findLoops 1 pointsBetween M.empty

    addStraightPaths = M.unionWith (++) loops $ M.fromListWith (++) keyz
      where
        keys = sort $ map snd $ s : e : M.keys loops
        keyz = concat $ zipWith fn keys (tail keys)
        fn :: Int -> Int -> [(Point2d, [Edge])]
        fn a b =
          [ ((row, a), [Edge (row, b) East (b - a) 0 0]),
            ((row, b), [Edge (row, a) West (b - a) 0 0])
          ]

    findUpLoop = findLoop [first pred, second succ, first succ]
    findDownLoop = findLoop [first succ, second succ, first pred]

    findLoop :: [Point2d -> Point2d] -> Point2d -> (Point2d, (Int, Int))
    findLoop moveFns startP = go (0, 0) (startP, '.', moveFns)
      where
        go :: (Int, Int) -> (Point2d, Char, [Point2d -> Point2d]) -> (Point2d, (Int, Int))
        go acc (p, v, moveFns@(mfn : moveFns')) =
          if fst p' == row
            then (p', acc')
            else case M.lookup p' g of
              Nothing -> go acc (p, v, moveFns')
              Just v' -> go acc' (p', v', moveFns)
          where
            p' = mfn p
            acc' = bimap succ (+ if v == '@' then 1 else 0) acc

    findLoops ::
      Int ->
      [Point2d] ->
      M.HashMap Point2d [Edge] ->
      M.HashMap Point2d [Edge]
    findLoops _ [] loops = loops
    findLoops i (p : ps) loops
      | p `M.member` loops = findLoops i ps loops
      | not (first succ p `M.member` g) = findLoops i ps loops
      | p' /= q' = error "Up and down loops do not match"
      | otherwise = findLoops (i + 2) ps (M.union loops' loops)
      where
        loops' =
          M.filter (not . null) $
            M.map (filter ((> 0) . (^. sulferCount))) $
              M.fromList
                [ (p, [Edge p' North upw upn i, Edge p' South dnw dnn (i + 1)]),
                  (p', [Edge p North upw upn i, Edge p South dnw dnn (i + 1)])
                ]
        (p', (upw, upn)) = findUpLoop p
        (q', (dnw, dnn)) = findDownLoop p

findPathP1 :: Graph -> Int
findPathP1 graph = fst res
  where
    Just res = dijkstraAssoc next goal (s, ((-1, -1), East), S.empty)

    s = graph ^. start
    e = graph ^. end
    cg = graph ^. grid

    target = S.fromList $ map (^. sulferIx) $ concat $ M.elems cg

    next ::
      (Point2d, (Point2d, Direction), S.Set Int) ->
      [((Point2d, (Point2d, Direction), S.Set Int), Int)]
    next (p, q, n) =
      [ ((p', (p, dir), S.insert sulferIx n), cost)
      | Edge p' dir cost _ sulferIx <- cg M.! p,
        (p', dir) /= q
      ]

    goal (p, _, n) = p == e && n == target

findPathP23 :: (Int -> Bool) -> Graph -> Int
findPathP23 targetFn graph = fst res
  where
    Just res = dijkstraAssoc next goal (s, ((-1, -1), East), 0)

    s = graph ^. start
    e = graph ^. end
    cg = graph ^. grid

    next ::
      (Point2d, (Point2d, Direction), Int) ->
      [((Point2d, (Point2d, Direction), Int), Int)]
    next (p, q, n) =
      [ ((p', (p, dir), sulfer + n), cost)
      | Edge p' dir cost sulfer sulferIx <- cg M.! p,
        (p', dir) /= q
      ]

    goal (p, _, n) = p == e && targetFn n

betterLoop :: Loop -> Loop -> Bool
betterLoop a b =
  toInteger (loopSulferCount a) * toInteger (loopWeight b)
    > toInteger (loopSulferCount b) * toInteger (loopWeight a)

findBestLoop :: Graph -> Loop
findBestLoop graph = foldl1 chooseBetter $ concatMap go xs
  where
    s = graph ^. start
    e = graph ^. end
    cg = graph ^. grid

    xs =
      [ (fst s, c)
      | c <- [snd s + 1 .. snd e - 1]
      ]

    chooseBetter a b
      | betterLoop a b = a
      | otherwise = b

    opposite = \case
      North -> South
      South -> North
      East -> West
      West -> East

    go :: Point2d -> [Loop]
    go start = M.elems results
      where
        initialState = (start, East)

        initialQueue = S.singleton (0, 0, initialState, 0, [])

        results = search initialQueue S.empty M.empty 1

        search queue seen best serial =
          case S.minView queue of
            Nothing -> best
            Just ((weightAcc, _, state@(current, _), sulfurAcc, path), queue')
              | state `S.member` seen ->
                  search queue' seen best serial
              | current == start && sulfurAcc > 0 ->
                  let loop = Loop weightAcc sulfurAcc (reverse path)
                      best' = M.insertWith chooseCheaper sulfurAcc loop best
                   in search queue' (S.insert state seen) best' serial
              | otherwise ->
                  let seen' = S.insert state seen
                      edges = M.lookupDefault [] current cg
                      (queue'', serial') =
                        foldl'
                          (explore current state weightAcc sulfurAcc path)
                          (queue', serial)
                          edges
                   in search queue'' seen' best serial'

        explore current (_, heading) weightAcc sulfurAcc path (queue, serial) edge
          | _direction edge == opposite heading =
              (queue, serial)
          | otherwise =
              let next = _to edge
                  reverseEdge =
                    case [ rev
                         | rev <- M.lookupDefault [] next cg,
                           _to rev == current,
                           _sulferIx rev == _sulferIx edge
                         ] of
                      rev : _ -> rev
                      [] -> error "Missing reverse edge"

                  nextHeading = opposite (_direction reverseEdge)
                  nextState = (next, nextHeading)

                  nextWeight = weightAcc + _weight edge
                  nextSulfur = sulfurAcc + _sulferCount edge

                  item =
                    (nextWeight, serial, nextState, nextSulfur, edge : path)
               in (S.insert item queue, serial + 1)

        chooseCheaper new old
          | loopWeight new < loopWeight old = new
          | otherwise = old
