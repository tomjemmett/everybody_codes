{-# LANGUAGE TemplateHaskell #-}

module Stories.S2.Quest03 where

import Common
import Control.Lens (makeLenses, (&), (.~), (^.))
import Control.Monad (forM, guard)
import Control.Monad.RWS
import Control.Parallel.Strategies
import Data.Char (digitToInt)
import Data.Function (on)
import Data.HashSet qualified as S
import Data.Hashable
import Data.List (intercalate, sortBy)
import Data.Vector qualified as V
import ECSolution (getInput)
import Text.Parsec qualified as P
import Text.Parsec.String (Parser)

data Die = Die
  { _seed :: Int,
    _faces :: V.Vector Int,
    _facesIdx :: Int,
    _pulse :: Int,
    _rollNumber :: Int
  }
  deriving (Show, Eq)

makeLenses ''Die

instance Hashable Die where
  hashWithSalt salt (Die {..}) =
    salt
      `hashWithSalt` _seed
      `hashWithSalt` V.toList _faces
      `hashWithSalt` _facesIdx
      `hashWithSalt` _pulse
      `hashWithSalt` _rollNumber

makeDie :: Int -> [Int] -> Die
makeDie seed faces = Die seed (V.fromList faces) 0 seed 1

roll :: Die -> (Int, Die)
roll Die {..} = (v, die')
  where
    spin = _rollNumber * _pulse
    p' = (_pulse + spin) `mod` _seed
    r' = _rollNumber + 1
    p'' = p' + r' + _seed
    i' = (_facesIdx + spin) `mod` V.length _faces
    v = _faces V.! i'
    die' = Die _seed _faces i' p'' r'

quest03 :: String -> IO (Int, String, Int)
quest03 = getInput 2 3 part1 part2 part3

part1 :: String -> Int
part1 input = pred $ go (0, dice)
  where
    dice = parse parseDice input
    go (t, ds) | t >= 10000 = head ds ^. rollNumber
    go ds = go $ rollDice ds

part2 :: String -> String
part2 input = intercalate "," $ map (show . fst) $ sortBy (compare `on` snd) $ zip [1 ..] rs
  where
    rs = map (go target) dice
    (dice, target) = parse ((,) <$> (parseDice <* P.newline) <*> parseDigitLine) input
    go :: [Int] -> Die -> Int
    go [] d = d ^. rollNumber
    go (t : ts) d = go (if r == t then ts else t : ts) d'
      where
        (r, d') = roll d

part3 :: String -> Int
part3 input = S.size . S.unions $ parMap rdeepseq go dice
  where
    p = do
      d <- parseDice <* P.newline
      t <- V.fromList . map V.fromList <$> parseDigitLine `P.sepBy` P.newline
      pure (d, t)
    (dice, target) = parse p input
    nrow = V.length target
    ncol = V.length (target V.! 0)
    start = [(r, c) | r <- [0 .. nrow - 1], c <- [0 .. ncol - 1]]
    go die = snd $ evalRWS (play (die, start)) (target, (nrow, ncol)) S.empty

play :: (Die, [Point2d]) -> RWS (V.Vector (V.Vector Int), Point2d) (S.HashSet Point2d) (S.HashSet (Int, Point2d)) ()
play (_, []) = pure ()
play (die, todo) = do
  (target, (nrow, ncol)) <- ask
  let (rv, die') = roll die
  next <- forM todo $ \(r, c) -> do
    m <- get
    let k = (die' ^. rollNumber, (r, c))
    if not (S.member k m)
      then do
        modify $ S.insert k
        if rv == target V.! r V.! c
          then do
            tell (S.singleton (r, c))
            pure
              [ (r + dr, c + dc)
              | (dr, dc) <- [(-1, 0), (1, 0), (0, -1), (0, 1), (0, 0)],
                r + dr >= 0,
                r + dr < nrow,
                c + dc >= 0,
                c + dc < ncol
              ]
          else pure []
      else pure []
  play (die', concat next)

parseInput :: String -> [Die]
parseInput = parse (p `P.sepBy` P.newline)
  where
    p = do
      die <- number
      P.string ": faces="
      faces <- P.between (P.char '[') (P.char ']') (number `P.sepBy` P.char ',')
      seed <- P.string " seed=" *> number'
      pure $ makeDie seed faces

parseDice :: Parser [Die]
parseDice = parseDie `P.sepEndBy` P.newline

parseDie :: Parser Die
parseDie = do
  number
  P.string ": faces="
  faces <- P.between (P.char '[') (P.char ']') (number `P.sepBy` P.char ',')
  seed <- P.string " seed=" *> number'
  pure $ makeDie seed faces

parseDigitLine :: Parser [Int]
parseDigitLine = map digitToInt <$> P.many P.digit

rollDice :: (Int, [Die]) -> (Int, [Die])
rollDice (currentScore, dice) = (currentScore + score, dice')
  where
    next = map roll dice
    dice' = map snd next
    score = sum $ map fst next
