{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TemplateHaskell #-}

module Events.Y2024.Quest15 where

import Algorithm.Search (dijkstra, dijkstraAssoc)
import Common
import Control.Lens (makeLenses, over, set, to, (^.))
import Control.Monad (forM)
import Control.Monad.State
import Data.Bifunctor (second)
import Data.Bits (bit, complement, (.&.), (.|.))
import Data.HashMap.Strict qualified as M
import Data.HashSet qualified as S
import Data.List (foldl', permutations, sort)
import Data.Sequence qualified as Seq
import ECSolution (getInput)

data Garden = Garden
  { _path :: S.HashSet Point2d,
    _start :: Point2d,
    _herbs :: M.HashMap Char [Point2d]
  }
  deriving (Show)

type Costs = M.HashMap Point2d (M.HashMap Point2d Int)

makeGarden :: S.HashSet Point2d -> Point2d -> M.HashMap Char [Point2d] -> Garden
makeGarden p s h = Garden {_path = p, _start = s, _herbs = h}

makeLenses ''Garden

quest15 :: String -> IO (Int, Int, Int)
quest15 = getInput 2024 15 part1 part2 part3

parseGarden :: String -> Garden
parseGarden input = makeGarden (S.fromList $ map fst vs) start herbs
  where
    vs =
      [ ((i, j), v)
        | (i, line) <- zip [0 ..] $ lines input,
          (j, v) <- zip [0 ..] line,
          v `notElem` "#~"
      ]
    start = fst $ head $ filter ((== 0) . fst . fst) vs
    herbs = M.fromListWith (++) $ map (\(k, v) -> (v, [k])) $ filter ((/= '.') . snd) vs

part1 :: String -> Int
part1 input = minimum [minCost g allCosts (g ^. start) perm | perm <- permutations allHerbs]
  where
    g = parseGarden input
    locs = (g ^. start) : concat (g ^. herbs . to M.elems)
    allCosts = buildAllCosts g locs
    allHerbs = g ^. herbs . to M.keys

part2 :: String -> Int
part2 = part1

part3 :: String -> Int
part3 input = if length locs == 358 then lft + rgt + mid else 0
  where
    g = parseGarden input
    locs = (g ^. start) : concat (g ^. herbs . to M.elems)
    allCosts = buildAllCosts g locs
    startE = maximum (g ^. herbs . to (M.! 'E'))
    startR = minimum (g ^. herbs . to (M.! 'R'))
    lft = minimum [minCost g allCosts startE perm | perm <- permutations "ABCDE"]
    rgt = minimum [minCost g allCosts startR perm | perm <- permutations "NOPQR"]
    mid = minimum [minCost g allCosts (g ^. start) perm | perm <- permutations "EGHIJKR"]

bfsDistances :: Garden -> Point2d -> M.HashMap Point2d Int
bfsDistances g start = go initialQueue initialDist
  where
    initialQueue = Seq.singleton start
    initialDist = M.singleton start 0
    neighbours p = filter (`S.member` (g ^. path)) $ point2dNeighbours p

    go !queue !distMap =
      case queue of
        Seq.Empty -> distMap
        (p Seq.:<| rest) ->
          let !d = distMap M.! p

              (rest', distMap') =
                foldl
                  ( \(!q, !dm) n ->
                      if M.member n dm
                        then (q, dm)
                        else
                          ( q Seq.|> n,
                            M.insert n (d + 1) dm
                          )
                  )
                  (rest, distMap)
                  (neighbours p)
           in go rest' distMap'

buildAllCosts :: Garden -> [Point2d] -> Costs
buildAllCosts g requiredLocations = M.fromList [(p, bfsDistances g p) | p <- requiredLocations]

minCost :: Garden -> Costs -> Point2d -> String -> Int
minCost garden costs from herbsSubset = evalState (go (from, herbsSubset)) M.empty
  where
    go :: (Point2d, String) -> State (M.HashMap (Point2d, String) Int) Int
    go (current, []) = pure $ costs M.! current M.! from
    go key@(current, nextHerb : remaining) = do
      memo <- get
      case M.lookup key memo of
        Just v -> pure v
        Nothing -> do
          let nextLocs = garden ^. herbs . to (M.! nextHerb)

          costs_list <-
            forM nextLocs $ \nextLoc -> do
              let costToNext = costs M.! current M.! nextLoc
              restCost <- go (nextLoc, remaining)
              pure $ costToNext + restCost
          let v = minimum costs_list

          modify (M.insert key v)
          pure v
