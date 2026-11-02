module Events.Y2024.Quest10 where

import Common
import Control.Monad (replicateM)
import Data.Char (ord)
import Data.HashMap.Strict qualified as M
import Data.List (sort, transpose)
import Data.Maybe (mapMaybe)
import ECSolution (getInput)
import Text.Parsec qualified as P
import Text.Parsec.String (Parser)

gridChar :: [Char]
gridChar = '?' : '*' : '.' : ['A' .. 'Z']

quest10 :: String -> IO (String, Int, Int)
quest10 = getInput 2024 10 p1 p2 p3
  where
    p1 = part1 <$> parse pGrid
    p2 = part2 <$> parse pGrids . (++ "\n\n\n")
      where
        pGrids =
          do
            let lp = P.many1 (P.oneOf gridChar) `P.sepBy` P.char ' ' <* P.try P.newline
            v <- map makeGrid . transpose <$> replicateM 9 lp
            n <- P.try pGrids P.<|> pure []
            pure $ v ++ n
    p3 = part3 <$> parse pGrid

pGrid :: Parser (M.HashMap Point2d Char)
pGrid = makeGrid <$> P.many1 (P.noneOf "\n") `P.sepBy` P.newline

pGrids :: Parser [M.HashMap Point2d Char]
pGrids =
  do
    let lp = P.many1 (P.oneOf gridChar) `P.sepBy` P.char ' ' <* P.try P.newline
    v <- map makeGrid . transpose <$> replicateM 9 lp
    n <- P.try pGrids P.<|> pure []
    pure $ v ++ n

makeGrid :: [String] -> M.HashMap Point2d Char
makeGrid s =
  M.fromList
    [ ((i, j), v)
    | (i, line) <- zip [0 ..] s,
      (j, v) <- zip [0 ..] line,
      v /= '*'
    ]

part1 :: M.HashMap Point2d Char -> String
part1 = map snd . sort . solve (0, 0)

part2 :: [M.HashMap Point2d Char] -> Int
part2 = sum . map (sum . zipWith (*) [1 ..] . map (subtract 64 . ord) . part1)

part3 :: M.HashMap Point2d Char -> Int
part3 m = if m == m' then getScores m else part3 m'
  where
    m' = findMissings m

getScores :: M.HashMap Point2d Char -> Int
getScores m = sum $ map (getScore m) offsets
  where
    [mi, mj] = map maximum (map <$> [fst, snd] <*> pure (M.keys m))
    offsets = [(i, j) | i <- [0, 6 .. (mi - 6)], j <- [0, 6 .. (mj - 6)]]
    getScore :: M.HashMap Point2d Char -> Point2d -> Int
    getScore m (oi, oj)
      | length vs /= 16 = 0
      | otherwise = sum . zipWith (*) [1 ..] $ map (subtract 64 . ord) vs
      where
        vs = filter (/= '.') [m M.! (i + oi, j + oj) | i <- [2 .. 5], j <- [2 .. 5]]

getKeys :: Point2d -> [Point2d]
getKeys (oi, oj) =
  concat
    [ [(oi + x, oj + y), (oi + y, oj + x)]
    | x <- [2 .. 5],
      y <- [0, 1, 6, 7]
    ]

getDots :: Point2d -> [Point2d]
getDots (oi, oj) = [(oi + i, oj + j) | i <- [2 .. 5], j <- [2 .. 5]]

findMissings :: M.HashMap Point2d Char -> M.HashMap Point2d Char
findMissings m = foldl (flip findMissing) m offsets
  where
    [mi, mj] = map maximum (map <$> [fst, snd] <*> pure (M.keys m))
    offsets = [(i, j) | i <- [0, 6 .. (mi - 6)], j <- [0, 6 .. (mj - 6)]]

findMissing :: Point2d -> M.HashMap Point2d Char -> M.HashMap Point2d Char
findMissing offset@(oi, oj) m =
  if isUnsolvable
    then m
    else M.union (M.fromList (concatMap fm qs)) m'
  where
    ks = getKeys offset
    s = solve offset m
    m' = M.union (M.fromList s) m
    qs = [k | k <- ks, m M.! k == '?']
    isUnsolvable :: Bool
    isUnsolvable = any checkV [2 .. 5] || any checkH [2 .. 5]
      where
        checkH i = checkVH [i] [0, 1, 6, 7]
        checkV j = checkVH [0, 1, 6, 7] [j]
        checkVH is js =
          let [a, b, c, d] = [m M.! (oi + i, oj + j) | i <- is, j <- js]
           in a == c || a == d || b == c || b == d
    fm :: Point2d -> [(Point2d, Char)]
    fm (i, j) = if (i - oi) > 1 && (i - oi) < 6 then fmh else fmv
      where
        go :: [Int] -> (Int -> [(Char, Int)]) -> Maybe (Char, Int)
        go dss vss
          | null dss = Nothing
          | null ds && null vs = Just (v, d)
          | otherwise = Nothing
          where
            (d : ds) = dss
            (v : vs) = M.keys $ M.filter (== 1) $ M.fromListWith (+) $ filter ((/= '.') . fst) $ vss d
        fmv :: [(Point2d, Char)]
        fmv = case go dss vss of
          Just (v, d) -> [((i, j), v), ((d, j), v)]
          Nothing -> []
          where
            dss = [oi + x | x <- [2 .. 5], m' M.! (oi + x, j) == '.']
            vss d = [(m' M.! (d, oj + j'), 1) | j' <- [0 .. 7]]
        fmh :: [(Point2d, Char)]
        fmh = case go dss vss of
          Just (v, d) -> [((i, j), v), ((i, d), v)]
          Nothing -> []
          where
            dss = [oj + x | x <- [2 .. 5], m' M.! (i, oj + x) == '.']
            vss d = [(m' M.! (oi + i', d), 1) | i' <- [0 .. 7]]

solve :: Point2d -> M.HashMap Point2d Char -> [(Point2d, Char)]
solve offset@(oi, oj) m = mapMaybe fn dots
  where
    dots = filter ((== '.') . (m M.!)) $ getDots offset
    fn (i, j) = case filter (`elem` b) a of
      [x] -> Just ((i, j), x)
      _ -> Nothing
      where
        a = [m M.! (x + oi, j) | x <- [0, 1, 6, 7]]
        b = [m M.! (i, x + oj) | x <- [0, 1, 6, 7]]
