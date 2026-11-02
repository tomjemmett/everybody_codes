module Events.Y2024.Quest11 where

import Common
import Control.Monad.RWS
import Data.HashMap.Strict qualified as M
import ECSolution (getInput)
import Text.Parsec qualified as P
import Text.Parsec.String (Parser)

type Termites = M.HashMap String Int

type Rules = M.HashMap String [String]

quest11 :: String -> IO (Int, Int, Int)
quest11 = getInput 2024 11 p1 p2 p3
  where
    p1 = part1 . parse pLines
    p2 = part2 . parse pLines
    p3 = part3 . parse pLines
    pLines = M.fromList <$> pLine `P.sepEndBy` P.newline
    pLine = do
      x <- P.many1 P.letter <* P.char ':'
      ys <- P.many1 P.letter `P.sepBy` P.char ','
      pure (x, ys)

part1 :: Rules -> Int
part1 = run 4 "A"

part2 :: Rules -> Int
part2 = run 10 "Z"

part3 :: Rules -> Int
part3 rules =
  let vs = map (flip (run 20) rules) $ M.keys rules
   in maximum vs - minimum vs

run :: Int -> String -> Rules -> Int
run i k rules = (!! pred i) $ snd $ execRWS step rules (M.singleton k 1)

step :: RWS Rules [Int] Termites ()
step = do
  xs <- concat <$> (gets M.toList >>= traverse (\(t, n) -> map (,n) . (M.! t) <$> ask))
  put $ M.fromListWith (+) xs
  tell [sum $ map snd xs]
  step
