{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TemplateHaskell #-}

module Events.Y2026.Quest13 where

import Common hiding (Direction)
import Control.Lens
import Data.List (foldl', tails, zipWith4)
import ECSolution (getInput)
import Text.Parsec qualified as P
import Text.Parsec.String (Parser)

data Direction = CW | CCW deriving (Show)

data ClockHand = ClockHand
  { _hand :: Int,
    _direction :: Direction,
    _position :: Int,
    _move :: Int
  }

makeLenses ''ClockHand

instance Show ClockHand where
  show (ClockHand h d p m) =
    "Hand (h="
      ++ show h
      ++ ", d="
      ++ show d
      ++ ", p="
      ++ show p
      ++ ", m="
      ++ show m
      ++ ")"

type Clock = [ClockHand]

quest13 :: String -> IO (Int, Int, Int)
quest13 = getInput 2026 13 part1 part2 part3

part1 :: String -> Int
part1 = sum . map go . parseInput

part2 :: String -> Int
part2 = part1

part3 :: String -> Int
part3 = part1

parseInput :: String -> [Clock]
parseInput = parse (P.many parseClock)
  where
    parseClock :: Parser Clock
    parseClock = do
      P.many P.space
      h <- P.string "hands " *> number `P.sepBy` P.char ':'
      d <- P.string "direction " *> direction `P.sepBy` P.char ':' <* P.endOfLine
      c <- P.string "current " *> number `P.sepBy` P.char ':'
      m <- P.string "move " *> number `P.sepBy` P.char ':'
      pure $ zipWith4 ClockHand h d c m
    direction :: Parser Direction
    direction =
      (CW <$ P.try (P.string "CW"))
        P.<|> (CCW <$ P.string "CCW")

moveClockHand :: ClockHand -> ClockHand
moveClockHand clockHand
  | clockHand ^. move <= 0 = clockHand
  | otherwise =
      clockHand
        & position %~ (`mod` (clockHand ^. hand)) . (+ delta)
        & move -~ 1
  where
    delta = case clockHand ^. direction of
      CW -> 1
      CCW -> -1

lcm' :: [Int] -> Int
lcm' = foldl' lcm 1

positionAndVelocity :: Int -> ClockHand -> (Int, Int)
positionAndVelocity circumference clockHand = (p, v)
  where
    scale = circumference `div` (clockHand ^. hand)
    p = clockHand ^. position * scale
    v
      | clockHand ^. move <= 0 = 0
      | otherwise =
          case clockHand ^. direction of
            CW -> scale
            CCW -> -scale

ceilDiv :: Int -> Int -> Int
ceilDiv a b = negate ((negate a) `div` b)

crossings :: Int -> ClockHand -> ClockHand -> Int
crossings circumference a b
  | dv > 0 = r1 `div` circumference - r0 `div` circumference
  | dv < 0 = ceilDiv r0 circumference - ceilDiv r1 circumference
  | otherwise = 0
  where
    (p1, v1) = positionAndVelocity circumference a
    (p2, v2) = positionAndVelocity circumference b

    r0 = p1 - p2
    dv = v1 - v2
    r1 = r0 + dv

countCrossings :: Int -> Clock -> Int
countCrossings circumference clock =
  sum
    [ crossings circumference a b
    | (a : rest) <- tails clock,
      b <- rest
    ]

signalsDuring :: Int -> Int -> Clock -> Int
signalsDuring n circumference = go n 0
  where
    go 0 !total _ = total
    go remaining !total clock =
      let !total' = total + countCrossings circumference clock
       in go (remaining - 1) total' (map moveClockHand clock)

go :: Clock -> Int
go clock
  | null active = 0
  | cycles > 0 =
      cycles * cycleSignals
        + go clockAfterCycles
  | otherwise =
      countCrossings
        circumference
        clock
        + go (map moveClockHand clock)
  where
    active = filter ((> 0) . (^. move)) clock
    circumference = foldl1 lcm (map (^. hand) clock)
    period = lcm' (map (^. hand) active)
    minMoves = minimum (map (^. move) active)
    cycles = minMoves `div` period
    cycleSignals = signalsDuring period circumference clock
    clockAfterCycles =
      map (\h -> h & move -~ (cycles * period)) clock