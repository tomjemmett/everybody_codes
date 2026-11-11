module Stories.S4.Quest03 where

import Common
import Data.Bifunctor (first, second)
import Data.Bits (xor)
import Data.Char (digitToInt)
import Data.HashMap.Strict qualified as M
import ECSolution (getInput)
import Text.Parsec qualified as P
import Text.Parsec.String (Parser)

quest03 :: String -> IO (Int, Int, Int)
quest03 = getInput 4 3 part1 part2 part3

part1 :: String -> Int
part1 = uncurry (+) . isolateCells . parseInput

part2 :: String -> Int
part2 = uncurry max . isolateCells . parseInput

part3 :: String -> Int
part3 = part2

parseInput :: String -> (Int, Int, [Int], [Int])
parseInput = parse do
  width <- P.string "width=" *> number
  height <- P.string "height=" *> number
  horizontalOffsets <- P.string "horizontal-offsets=" *> P.many1 P.digit
  P.endOfLine
  verticalOffsets <- P.string "vertical-offsets=" *> P.many1 P.digit
  pure
    (width, height, map digitToInt horizontalOffsets, map digitToInt verticalOffsets)

classify :: Int -> [Int] -> Maybe Int -> M.HashMap (Int, Int, Int) Int
classify size offsets target = foldl' f M.empty [0 .. limit - 1]
  where
    period = 2 * length offsets
    values = take (period + 1) $ cycle offsets
    prefix = makePrefix values
    limit = min size period

    values' = M.fromList $ zip [0 ..] values

    f b s
      | values' M.! s /= values' M.! (s + 1) = b
      | otherwise = M.insertWith (+) key count b
      where
        v = values' M.! s
        crossingTarget = maybe v id target
        crossings = (prefix M.! crossingTarget) !! s `mod` 2
        key = (v, s `mod` 2, crossings)
        count = (size - 1 - s) `div` period + 1

makePrefix :: [Int] -> M.HashMap Int [Int]
makePrefix values =
  M.fromList
    [ (value, scanl (\acc x -> acc `xor` fromEnum (x == value)) 0 (drop 1 values))
    | value <- [0, 1]
    ]

isolateCells :: (Int, Int, [Int], [Int]) -> (Int, Int)
isolateCells (width, height, horizontalOffsets, verticalOffsets) = goR rows (0, 0)
  where
    rows = M.toList $ classify height horizontalOffsets Nothing
    cols = M.toList $ classify width verticalOffsets (Just 0)

    goR :: [((Int, Int, Int), Int)] -> (Int, Int) -> (Int, Int)
    goR [] groups = groups
    goR (((hv, rp, hc), rn) : restR) groups =
      goR restR (goC cols rn groups)
      where
        goC :: [((Int, Int, Int), Int)] -> Int -> (Int, Int) -> (Int, Int)
        goC [] _ groups = groups
        goC (((vv, cp, vc), cn) : restC) rn groups
          | cp /= hv = goC restC rn groups
          | rp /= vv = goC restC rn groups
          | otherwise =
              goC restC rn $
                (if even (hc + vc) then first else second)
                  (+ rn * cn)
                  groups
