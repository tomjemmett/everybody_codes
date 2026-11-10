module Events.Y2026.Quest11 where

import Common
import Data.HashMap.Strict qualified as M
import Data.Sequence qualified as Q
import ECSolution (getInput)
import Text.Parsec qualified as P
import Text.Parsec.String (Parser)

quest11 :: String -> IO (Int, Int, Int)
quest11 = getInput 2026 11 part1 part2 part3

part1 :: String -> Int
part1 input = f - g
  where
    (n, paths, shortcuts) = parseInput 1 input
    pathsWithShortcuts = M.unionWith (++) paths shortcuts
    xs = [0 .. pred n]
    f = strain paths xs
    g = strain pathsWithShortcuts xs

part2 :: String -> Int
part2 input = f - g
  where
    (n, paths, shortcuts) = parseInput 3 input
    pathsWithShortcuts = M.unionWith (++) paths shortcuts
    xs = [0 .. pred n]
    f = strain paths xs
    g = minimum $ map (`strain` xs) pathsWithExtraShortcut

    pathsWithExtraShortcut =
      [ M.unionWith (++) pathsWithShortcuts (M.fromList [(h1, [h2]), (h2, [h1])])
      | h1 <- xs,
        h2 <- xs,
        h2 `notElem` (pathsWithShortcuts M.! h1)
      ]

pairings :: [a] -> [[(a, a)]]
pairings [] = [[]]
pairings (x : xs) =
  [ (x, y) : rest
  | (y, remaining) <- pick xs,
    rest <- pairings remaining
  ]
  where
    pick [] = []
    pick (y : ys) =
      (y, ys) : [(z, y : zs) | (z, zs) <- pick ys]

part3 :: String -> Int
part3 input = f - g
  where
    (n, paths, shortcuts) = parseInput 3 input
    pathsWithShortcuts = M.unionWith (++) paths shortcuts
    xs = [0 .. pred n]

    f = strain paths xs

    g =
      minimum
        [ strain (addConnections pathsWithShortcuts pairs) xs
        | offset <- [0 .. spacing - 1],
          let positions =
                [ (offset + i * spacing) `mod` n
                | i <- [0 .. 7]
                ],
          pairs <- pairings positions
        ]

    spacing = n `div` 8

    addConnections graph pairs =
      M.unionWith
        (++)
        graph
        ( M.fromListWith
            (++)
            [ edge
            | (a, b) <- pairs,
              edge <- [(a, [b]), (b, [a])]
            ]
        )

parseInput :: Int -> String -> (Int, M.HashMap Int [Int], M.HashMap Int [Int])
parseInput size = parse do
  P.string "portals: "
  n <- number

  let paths = [(i, [(i - j) `mod` n]) | i <- [0 .. (n - 1)], j <- [-size .. size], j /= 0]
  shortcuts <- p `P.sepEndBy` P.endOfLine
  pure (n, M.fromListWith (++) paths, M.fromListWith (++) $ concat shortcuts)
  where
    p = do
      from <- pred <$> number
      to <- pred <$> number'
      pure [(from, [to]), (to, [from])]

shortestPaths :: M.HashMap Int [Int] -> Int -> M.HashMap Int Int
shortestPaths graph start = go (Q.singleton start) (M.singleton start 0)
  where
    go queue distances =
      case Q.viewl queue of
        Q.EmptyL -> distances
        node Q.:< rest -> go' node rest
      where
        go' node rest = go queue' distances'
          where
            distance = distances M.! node
            neighbours = M.findWithDefault [] node graph

            unseen = filter (not . (`M.member` distances)) neighbours

            distances' =
              foldr
                (\n -> M.insert n (distance + 1))
                distances
                unseen

            queue' = rest Q.>< Q.fromList unseen

strain :: M.HashMap Int [Int] -> [Int] -> Int
strain graph = sum . concatMap (M.elems . shortestPaths graph)