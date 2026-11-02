module Events.Y2024.Quest16 where

import Common
import Control.Lens (view)
import Control.Monad (forM)
import Control.Monad.State
import Data.Bifunctor (bimap)
import Data.HashMap.Strict qualified as M
import Data.List (transpose)
import Data.List.PointedList.Circular qualified as PL
import Data.List.Split (chunksOf)
import Data.Maybe (mapMaybe)
import ECSolution (getInput)
import Text.Parsec qualified as P
import Text.Parsec.String (Parser)

type Cats = [PL.PointedList String]

quest16 :: String -> IO (String, Int, String)
quest16 = getInput 2024 16 p1 p2 p3
  where
    p1 = part1 . parseInput id
    p2 = part2 . parseInput (\[x, _, y] -> [x, y])
    p3 = part3 . parseInput (\[x, _, y] -> [x, y])

parseInput :: (String -> String) -> String -> ([Int], Cats)
parseInput f = parse do
  xs <- number' `P.sepBy` P.char ','
  P.newline
  P.newline
  cats <- mapMaybe (PL.fromList . map f . filter (/= "   ")) . transpose . chunksOf (length xs) <$> pCats
  pure (xs, cats)
  where
    pCat :: Parser String
    pCat = P.count 3 P.anyChar
    pCats :: Parser [String]
    pCats = pCat `P.sepBy` (P.space <* P.optional P.newline)

pull :: [Int] -> Cats -> Cats
pull = go
  where
    go _ [] = []
    go (x : xs) (c : cats) = PL.moveN x c : go xs cats

result :: Cats -> [String]
result = map (view PL.focus)

score :: [String] -> Int
score = sum . filter (> 0) . map (subtract 2) . M.elems . M.fromListWith (+) . map (,1) . concat

part1 :: ([Int], Cats) -> String
part1 (xs, cats) = unwords $ result $ (!! 100) $ iterate (pull xs) cats

part2 :: ([Int], Cats) -> Int
part2 (xs, cats) = d * last scores + scores !! r
  where
    x = foldr1 lcm $ map PL.length cats
    (d, r) = 202420242024 `divMod` x
    catses = take x $ tail $ iterate (pull xs) cats
    scores = scanl (+) 0 $ map (score . result) catses

part3 :: ([Int], Cats) -> String
part3 (xs, cats) = unwords [show a, show b]
  where
    (a, b) = evalState (maxminScore xs ((0, 0, 256), cats)) M.empty

maxminScore :: [Int] -> ((Int, Int, Int), Cats) -> State (M.HashMap (Int, Int, Int) (Int, Int)) (Int, Int)
maxminScore xs (k@(wo, pn, pr), cats) = do
  let r = result cats
      s = if pn == 0 then 0 else score r
  if pr == 0
    then pure (s, s)
    else do
      m <- get
      case M.lookup k m of
        Just (mi, ma) -> pure (mi, ma)
        Nothing -> do
          let c = ((wo + 0, succ pn, pred pr), pull xs cats)
              d = ((wo - 1, succ pn, pred pr), pull xs $ pull (repeat (-1)) cats)
              e = ((wo + 1, succ pn, pred pr), pull xs $ pull (repeat 1) cats)
          r <- unzip <$> forM [c, d, e] (maxminScore xs)
          let v = bimap ((s +) . maximum) ((s +) . minimum) r
          modify $ M.insert k v
          pure v
