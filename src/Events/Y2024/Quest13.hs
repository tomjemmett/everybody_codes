module Events.Y2024.Quest13 where

import Algorithm.Search (aStar)
import Common
import Data.Bifunctor (second)
import Data.Char (digitToInt)
import Data.HashMap.Strict qualified as M
import Data.List (find, intercalate)
import ECSolution (getInput)
import Text.Parsec qualified as P
import Text.Parsec.String (Parser)

quest13 :: String -> IO (Int, Int, Int)
quest13 = getInput 2024 13 f f f
  where
    f = search . parse parseMaze

parseMaze :: Parser (M.HashMap Point2d Int, [Point2d], Point2d)
parseMaze = do
  lines <- P.many1 (P.oneOf " #0123456789SE") `P.sepEndBy` P.newline
  let vs =
        [ ((i, j), v)
        | (i, line) <- zip [0 ..] lines,
          (j, v) <- zip [0 ..] line,
          v `notElem` " #"
        ]
      s = map fst $ filter ((== 'S') . snd) vs
      Just (e, _) = find ((== 'E') . snd) vs
  pure (M.fromList $ map (second toInt) $ filter ((/= 'S') . snd) vs, s, e)
  where
    toInt = \case
      'S' -> 0
      'E' -> 0
      x -> digitToInt x

search :: (M.HashMap Point2d Int, [Point2d], Point2d) -> Int
search (g, s, e) = r
  where
    Just (r, _) = aStar neighbours cost heur goal Nothing
    --
    heur = \case
      Nothing -> 0
      Just p -> manhattanDistance p e
    --
    goal :: Maybe Point2d -> Bool
    goal = \case
      Nothing -> False
      Just p -> p == e
    --
    cost :: Maybe Point2d -> Maybe Point2d -> Int
    cost Nothing _ = 0
    cost _ Nothing = 0
    cost (Just a) (Just b) = 1 + minimum [abs (h a - h b + i) | i <- [0, 10, -10]]
      where
        h x = M.lookupDefault 0 x g
    --
    neighbours :: Maybe Point2d -> [Maybe Point2d]
    neighbours p = map Just $ case p of
      Nothing -> s
      Just p -> filter (`M.member` g) $ point2dNeighbours p
