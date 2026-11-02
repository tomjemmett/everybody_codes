module Events.Y2025.Quest02 where

import Common
import Data.Complex
import Data.Maybe (isJust)
import ECSolution (getInput)
import Text.Parsec qualified as P

asResult :: Complex Int -> String
asResult (x :+ y) = "[" ++ show x ++ "," ++ show y ++ "]"

quest02 :: String -> IO (String, Int, Int)
quest02 = getInput 2025 2 part1 part2 part3

addComplex :: Complex Int -> Complex Int -> Complex Int
addComplex (x1 :+ y1) (x2 :+ y2) = (x1 + x2) :+ (y1 + y2)

multiplyComplex :: Complex Int -> Complex Int -> Complex Int
multiplyComplex (x1 :+ y1) (x2 :+ y2) = (x1 * x2 - y1 * y2) :+ (x1 * y2 + y1 * x2)

divideComplex :: Complex Int -> Complex Int -> Complex Int
divideComplex (x1 :+ y1) (x2 :+ y2) = x :+ y
  where
    x = if x1 > 0 then x1 `div` x2 else -((-x1) `div` x2)
    y = if y1 > 0 then y1 `div` y2 else -((-y1) `div` y2)

part1 :: String -> String
part1 input = asResult r
  where
    a = parseComplex input
    Just r = iterate (cycleComplex a b) (Just $ 0 :+ 0) !! 3
    b = 10 :+ 10

part2 :: String -> Int
part2 = part2and3 [0, 10 .. 1001]

part3 :: String -> Int
part3 = part2and3 [0 .. 1000]

part2and3 :: [Int] -> String -> Int
part2and3 xs input = countTrue isJust as
  where
    a = parseComplex input
    as =
      [ fn $ addComplex a (i :+ j)
      | i <- xs,
        j <- xs
      ]
    fn a' = iterate (cycleComplex a' b) (Just $ 0 :+ 0) !! 100
    b = 100000 :+ 100000

parseComplex :: String -> Complex Int
parseComplex = parse do
  x <- P.many1 P.letter *> P.string "=[" *> number
  y <- P.char ',' *> number <* P.char ']'
  pure (x :+ y)

cycleComplex :: Complex Int -> Complex Int -> Maybe (Complex Int) -> Maybe (Complex Int)
cycleComplex _ _ Nothing = Nothing
cycleComplex a b (Just r0) =
  if inRange r3
    then Just r3
    else Nothing
  where
    r1 = multiplyComplex r0 r0
    r2 = divideComplex r1 b
    r3 = addComplex r2 a
    inRange (x :+ y) = abs x <= 1000000 && abs y <= 1000000
