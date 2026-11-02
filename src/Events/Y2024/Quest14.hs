module Events.Y2024.Quest14 where

import Algorithm.Search (dijkstra)
import Common
import Data.HashSet qualified as S
import ECSolution (getInput)
import Text.Parsec qualified as P
import Text.Parsec.String (Parser)

quest14 :: String -> IO (Int, Int, Int)
quest14 = getInput 2024 14 p1 p2 p3
  where
    p1 = part1 . head <$> parseInput
    p2 = part2 <$> parseInput
    p3 = part3 . filter (not . null) <$> parseInput

parseInput :: String -> [[(Char, Int)]]
parseInput = parse (parseSchedule `P.sepBy` P.newline)

parseSchedule :: Parser [(Char, Int)]
parseSchedule = p `P.sepBy` P.char ','
  where
    p = (,) <$> P.oneOf "UDRLFB" <*> number'

part1 :: [(Char, Int)] -> Int
part1 = maximum . map (\(_, y, _) -> y) . runSchedule

part2 :: [[(Char, Int)]] -> Int
part2 = S.size . foldr (S.union . S.fromList . runSchedule) S.empty

part3 :: [[(Char, Int)]] -> Int
part3 schedules = minimum scores
  where
    segments :: [[Point3d]]
    segments = map runSchedule schedules
    segments' :: S.HashSet Point3d
    segments' = S.fromList $ concat segments
    leaves :: [Point3d]
    leaves = map last segments
    mainBranch :: [Point3d]
    mainBranch = S.toList $ S.filter (\(x, _, z) -> x == 0 && z == 0) segments'
    murkiness :: Point3d -> Point3d -> Int
    murkiness b l = r
      where
        Just (r, _) = dijkstra ns cost goal l
        ns = filter (`S.member` segments') . point3dNeighbours
        cost _ = const 1
        goal = (== b)
    score :: Point3d -> Int
    score b = sum $ map (murkiness b) leaves
    scores :: [Int]
    scores = map score mainBranch

runSchedule :: [(Char, Int)] -> [Point3d]
runSchedule = flip go (0, 0, 0)
  where
    go [] p = []
    go ((_, 0) : cs) p = go cs p
    go ((c, n) : cs) (x, y, z) = p' : go ((c, pred n) : cs) p'
      where
        p' = case c of
          'L' -> (pred x, y, z)
          'R' -> (succ x, y, z)
          'D' -> (x, pred y, z)
          'U' -> (x, succ y, z)
          'B' -> (x, y, pred z)
          'F' -> (x, y, succ z)
