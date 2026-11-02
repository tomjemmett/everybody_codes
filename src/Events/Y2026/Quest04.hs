{-# LANGUAGE TemplateHaskell #-}

module Events.Y2026.Quest04 where

import Common
import Control.Lens (makeLenses, (^.))
import Data.Bifunctor (first)
import Data.Function (on)
import Data.HashMap.Strict qualified as M
import Data.List (sort, sortBy)
import ECSolution (getInput)
import Text.Parsec qualified as P
import Text.Parsec.String (Parser)

data Result = Result
  { _personId :: Integer,
    _quest :: Integer,
    _part :: Integer,
    _time :: Integer
  }
  deriving (Show, Eq)

makeLenses ''Result

quest04 :: String -> IO (Integer, Integer, Integer)
quest04 = getInput 2026 4 part1 part2 part3

part1 :: String -> Integer
part1 = sum . zipWith (*) [1 ..] . sort . M.elems . parseInput
  where
    parseInput :: String -> M.HashMap Integer Integer
    parseInput = M.fromListWith (+) . map f . parse (parseResult `P.sepBy` P.endOfLine)
      where
        f r = (r ^. personId, r ^. time)

part2 :: String -> Integer
part2 = sum . zipWith (*) [1 ..] . map snd . sort . M.elems . parseInput
  where
    parseInput :: String -> M.HashMap Integer (Integer, Integer)
    parseInput = M.fromListWith (+.) . map f . parse (parseResult `P.sepBy` P.endOfLine)
      where
        f r = (r ^. personId, (-1, r ^. time))
        (+.) :: (Num a, Num b) => (a, b) -> (a, b) -> (a, b)
        (x1, y1) +. (x2, y2) = (x1 + x2, y1 + y2)

part3 :: String -> Integer
part3 =
  rank (-1, -1) 1
    . sortBy (flip compare)
    . M.elems
    . M.fromListWith (+)
    . concatMap calcScoreForQuest
    . M.elems
    . parseInput
  where
    rank :: (Integer, Integer) -> Integer -> [Integer] -> Integer
    rank _ _ [] = 0
    rank (pr, pv) r (v : vs) = (v * pr') + rank (pr', v) (succ r) vs
      where
        pr' = if pv == v then pr else r
    parseInput :: String -> M.HashMap Integer (M.HashMap Integer [(Integer, Integer)])
    parseInput = M.fromListWith (M.unionWith (++)) . map f . parse (parseResult `P.sepBy` P.endOfLine)
      where
        f r = (r ^. quest, M.singleton (r ^. part) [(r ^. personId, r ^. time)])
    calcScoreForQuest :: M.HashMap Integer [(Integer, Integer)] -> [(Integer, Integer)]
    calcScoreForQuest m = M.toList $ M.fromListWith (+) $ concat [p1s, p2s, p3s]
      where
        p1 = M.lookupDefault [] 1 m
        p1s = calc 50 p1
        p2 = getTotalTime (M.fromList p1) $ M.lookupDefault [] 2 m
        p2s = calc 100 p2
        p3 = getTotalTime (M.fromList p2) $ M.lookupDefault [] 3 m
        p3s = calc 150 p3
        getTotalTime prior = map (\(i, t) -> (i, t + M.findWithDefault 0 i prior))
    calc :: Integer -> [(Integer, Integer)] -> [(Integer, Integer)]
    calc s = f (-1, succ s) s . sortBy (compare `on` snd)
      where
        f :: (Integer, Integer) -> Integer -> [(Integer, Integer)] -> [(Integer, Integer)]
        f _ _ [] = []
        f (pt, ps) s ((i, t) : is) = v : f (pt', ps') s' is
          where
            v = (i, max 0 ps')
            s' = pred s
            pt' = t
            ps' = if t == pt then ps else s

parseResult :: Parser Result
parseResult = do
  id <- P.string "id=" *> number
  quest <- P.string "quest=" *> number
  part <- P.string "part=" *> number
  time <- P.string "time=" *> number'
  pure $ Result id quest part time
  where
    number :: Parser Integer
    number = lexeme number'
    number' :: Parser Integer
    number' =
      P.choice
        [ P.char '-' *> fmap negate digits,
          P.char '+' *> digits,
          digits
        ]
      where
        digits = read <$> P.many1 P.digit
