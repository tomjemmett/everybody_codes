{-# LANGUAGE BinaryLiterals #-}

module Events.Y2026.Quest20 where

import Common
import Data.Bits (bit, popCount, xor, (.&.), (.|.))
import Data.Char (digitToInt, isSpace)
import Data.List (foldl')
import Data.Maybe (fromMaybe, mapMaybe)
import ECSolution (getInput)

quest20 :: String -> IO (Int, Int, Int)
quest20 = getInput 2026 20 part1 part2 part3

part1 :: String -> Int
part1 = sum . mapMaybe solve . parseInput

part2 :: String -> Int
part2 = read . b11Show . sum . mapMaybe solve . parseInput

part3 :: String -> Int
part3 = read . b11Show . sum . map count . filter repairable . parseInput

parseInput :: String -> [String]
parseInput = map (filter (not . isSpace)) . lines

isCorrect :: String -> Bool
isCorrect xs =
  fromMaybe False $ do
    (a, op1 : rest) <- Just (break (`elem` "+-") xs)
    (b, _ : rhs) <- Just (break (== '=') rest)
    (c, op2 : d) <- Just (break (`elem` "+-") rhs)

    pure $
      apply op1 (b11Read a) (b11Read b)
        == apply op2 (b11Read c) (b11Read d)
  where
    apply '+' = (+)
    apply '-' = (-)

b11Read :: String -> Int
b11Read = foldl' (\acc x -> acc * 11 + digit x) 0
  where
    digit 'A' = 10
    digit x = digitToInt x

b11Show :: Int -> String
b11Show 0 = "0"
b11Show n = reverse $ go n
  where
    go 0 = []
    go x =
      let (q, r) = x `divMod` 11
       in ("0123456789A" !! r) : go q

-- Seven-segment layouts.
--
--       0
--     -----
--   5 |   | 1
--     --6--
--   4 |   | 2
--     -----
--       3

segmentMask :: Char -> Int
segmentMask = \case
  '0' -> 0b0111111
  '1' -> 0b0000110
  '2' -> 0b1011011
  '3' -> 0b1001111
  '4' -> 0b1100110
  '5' -> 0b1101101
  '6' -> 0b1111101
  '7' -> 0b0000111
  '8' -> 0b1111111
  '9' -> 0b1101111
  'A' -> 0b1110111
  '+' -> 0b11000000
  '-' -> 0b01000000
  '=' -> 0b11000000
  _ -> 0

segments :: Char -> Int
segments = popCount . segmentMask

count :: String -> Int
count = sum . map segments

validSymbols :: Char -> String
validSymbols c =
  if c `elem` "+-"
    then "+-"
    else "0123456789A"

options :: (Int -> Int -> Bool) -> Char -> String
options predicate c =
  [ x
  | x <- validSymbols c,
    let old = segmentMask c
        new = segmentMask x,
    predicate old new
  ]

removeOptions :: Char -> String
removeOptions = options $ \old new ->
  old .&. new == new && popCount old == popCount new + 1

addOptions :: Char -> String
addOptions = options $ \old new ->
  old .&. new == old && popCount new == popCount old + 1

moveWithin :: Char -> String
moveWithin c =
  [ x
  | x <- validSymbols c,
    let old = segmentMask c
        new = segmentMask x,
    popCount (old `xor` new) == 2,
    popCount old == popCount new
  ]

replaceAt :: Int -> Char -> String -> String
replaceAt i c xs =
  take i xs ++ [c] ++ drop (i + 1) xs

moveBetween :: String -> [(Int, Int, Char, Char)]
moveBetween xs =
  [ (i, j, x, y)
  | (i, a) <- zip [0 ..] xs,
    a /= '=',
    (j, b) <- zip [0 ..] xs,
    b /= '=',
    i /= j,
    x <- removeOptions a,
    y <- addOptions b
  ]

repairable :: String -> Bool
repairable xs =
  isCorrect xs || within || between
  where
    positions = [(i, c) | (i, c) <- zip [0 ..] xs, c /= '=']

    within =
      or
        [ isCorrect $ replaceAt i x xs
        | (i, c) <- positions,
          x <- moveWithin c
        ]

    between =
      or
        [ isCorrect $ replaceAt j y $ replaceAt i x xs
        | (i, j, x, y) <- moveBetween xs
        ]

solve :: String -> Maybe Int
solve xs =
  if isCorrect xs
    then Just $ count xs
    else Nothing
