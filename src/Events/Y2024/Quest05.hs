module Events.Y2024.Quest05 where

import Common
import Control.Monad.State
import Data.HashMap.Strict qualified as M
import Data.HashSet qualified as S
import Data.List (transpose)
import Data.Sequence (Seq ((:<|)))
import Data.Sequence qualified as SQ
import Data.Vector qualified as V
import ECSolution (getInput)

type Dancers = V.Vector (SQ.Seq Int)

type Shouts = [String]

quest05 :: String -> IO (String, String, String)
quest05 = getInput 2024 5 p1 p2 p3
  where
    [p1, p2, p3] = (.) <$> [part1, part2, part3] <*> [parseInput]

part1 :: Dancers -> String
part1 = shout . snd . (!! 10) . iterate (execState dance) . (0,)

part2 :: Dancers -> String
part2 i = show $ go 0 M.empty states
  where
    states = map (shout . snd) $ iterate (execState dance) (0, i)
    go r m (x : xs) = if m' M.! x == 2024 then r * read x else go (succ r) m' xs
      where
        m' = M.insertWith (+) x 1 m

part3 :: Dancers -> String
part3 = evalState (go S.empty "") . (0,)
  where
    go :: S.HashSet String -> String -> State (Int, Dancers) String
    go s m = do
      i <- get
      if show i `S.member` s
        then pure m
        else do
          dance
          i' <- get
          let s' = S.insert (show i) s
          go s' (max m $ shout (snd i'))

parseInput :: String -> Dancers
parseInput = V.fromList . map SQ.fromList . transpose . map (map read . words) . lines

dance :: State (Int, Dancers) ()
dance = do
  (i, v) <- get
  let (x :<| s) = v V.! i
      i' = succ i `mod` V.length v
  put (i', v V.// [(i, s)])
  shuffle x

shuffle :: Int -> State (Int, Dancers) ()
shuffle c = do
  (i, v) <- get
  let s = v V.! i
      l = SQ.length s
      n = (c - 1) `mod` (2 * l)
      n' = if n >= l then (2 * l) - n else n
  put (i, v V.// [(i, SQ.insertAt n' c s)])

shout :: Dancers -> String
shout = concatMap (\(x :<| _) -> show x)
