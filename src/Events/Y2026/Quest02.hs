module Events.Y2026.Quest02 where

import Common
import Data.HashMap.Strict qualified as M
import ECSolution (getInput)
import Text.Parsec qualified as P
import Text.Parsec.String (Parser)

quest02 :: String -> IO (Int, Int, Int)
quest02 = getInput 2026 2 solve solve solve

solve :: String -> Int
solve = sum . map getChecksum . parseInput

parseInput :: String -> [M.HashMap Int [Int]]
parseInput = fmap (M.fromListWith (++) . concat) . parse (p `P.sepBy` P.endOfLine)
  where
    p = pPair `P.sepBy` P.char ' '
    pPair = do
      i <- number
      c <- P.oneOf "<>"
      j <- number
      return $ if c == '<' then [(i, []), (j, [i])] else [(i, [j]), (j, [])]
    number = read <$> P.many1 P.digit

getChecksum :: M.HashMap Int [Int] -> Int
getChecksum = read . reverse . concat . go
  where
    go :: M.HashMap Int [Int] -> [String]
    go m | M.null m = []
    go m = show v : go m'
      where
        v = head $ M.keys $ M.filter (== []) m
        m' = M.map (filter (/= v)) $ M.delete v m
