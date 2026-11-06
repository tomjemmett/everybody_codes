{-# LANGUAGE TemplateHaskell #-}

module Events.Y2026.Quest08 where

import Common
import Control.Lens
import Control.Monad (forM_, guard, when)
import Control.Monad.State.Strict
import Data.Char (digitToInt, intToDigit)
import Data.HashMap.Strict qualified as M
import Data.List (find, groupBy, sort)
import Data.Ord (Down (Down), comparing)
import ECSolution (getInput)
import Text.Parsec qualified as P
import Text.Parsec.String (Parser)

data Field = Field
  { _grid :: M.HashMap Point2d Int,
    _energy :: Int,
    _warriors :: Int,
    _shadows :: [[[Point2d]]]
  }

makeLenses ''Field

instance Show Field where
  show (Field grid energy warriors _) = "Energy: " ++ show energy ++ "\nWarriors: " ++ show warriors ++ "\n\n" ++ gridS
    where
      maxRow = maximum $ map fst $ M.keys grid
      maxCol = maximum $ map snd $ M.keys grid
      gridS = unlines [[f (M.lookup (r, c) grid) | c <- [0 .. maxCol]] | r <- [0 .. maxRow]]
      f Nothing = '.'
      f (Just c) = if c < 1 then 'X' else intToDigit c

quest08 :: String -> IO (Int, Int, Int)
quest08 = getInput 2026 8 part1 part2 part3

part1 :: String -> Int
part1 input = state ^. energy
  where
    field = parseInput input
    state = execState runRound field

part2 :: String -> Int
part2 input = sum $ map (^. energy) states
  where
    field = parseInput input
    states = [execState runRound (over shadows (drop i) field) | i <- [0 .. 5]]

part3 :: String -> Int
part3 input = r ^. energy
  where
    field = parseInput input
    states = iterate (execState runRound) field
    Just r = find ((>= 100000) . (^. warriors)) states

parseInput :: String -> Field
parseInput input = Field grid 0 0 ss
  where
    grid = M.fromList $ do
      (r, line) <- zip [0 ..] (lines input)
      (c, char) <- zip [0 ..] line
      guard $ char /= '.'
      pure ((r, c), digitToInt char)
    ss = cycle $ getShadows grid

runRound :: State Field ()
runRound = do
  s <- get
  let xs : shadowsRest = s ^. shadows
  modify $ set shadows shadowsRest
  forM_ xs $ go3 0

go3 :: Int -> [Point2d] -> State Field ()
go3 _ [] = pure ()
go3 currentHeight (p : ps) = do
  s <- get
  let g = s ^. grid
  let height = g M.! p
  if (height < 1) || (currentHeight >= height)
    then do
      when (height < 1) do
        modify $ over grid (M.insert p (height + 1))
      go3 (max 0 $ currentHeight - 1) ps
    else do
      modify $ over energy (+ 1) . over grid (M.insert p (height + 1))
      when (height == 9) do
        modify $ over warriors (+ 1) . over grid (M.insert p (-2))
      go3 (height - 1) ps

getShadows :: M.HashMap Point2d Int -> [[[Point2d]]]
getShadows grid = [shadows1, shadows2, shadows3, shadows4, shadows5, shadows6]
  where
    coords = M.keys grid
    startCol = 0
    middleRow = minimum $ map fst $ filter ((== startCol) . snd) coords
    endCol = maximum $ map snd coords

    shadows1 :: [[Point2d]]
    shadows1 = f (middleRow, startCol)
      where
        f p@(r, c) = if null ps then [] else ps : f p'
          where
            ps = g p
            p' = (r - 1, c + 1)
        g p@(r, c) = takeWhile (`M.member` grid) [(r + i, c + i) | i <- [0 ..], r + i >= 0]

    shadows2 :: [[Point2d]]
    shadows2 = f (middleRow, endCol)
      where
        f p@(r, c) = if null ps then [] else ps : f p'
          where
            ps = g p
            p' = (r - 1, c - 1)
        g p@(r, c) = takeWhile (`M.member` grid) [(r + i, c - i) | i <- [0 ..], r + i >= 0]

    shadows3 :: [[Point2d]]
    shadows3 = map reverse shadows6

    shadows4 :: [[Point2d]]
    shadows4 = reverse $ (map reverse) shadows1

    shadows5 :: [[Point2d]]
    shadows5 = reverse $ (map reverse) shadows2

    shadows6 :: [[Point2d]]
    shadows6 = groupBy (\x y -> fst x == fst y) $ sort $ M.keys grid
