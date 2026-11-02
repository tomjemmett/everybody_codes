module Events.Y2026.Quest03 where

import Common 
import ECSolution (getInput)
import Text.Parsec qualified as P
import Text.Parsec.String (Parser)
import Data.Char (digitToInt)
import Data.List (find)
import Data.Maybe (isNothing, catMaybes)

quest03 :: String -> IO (Int, Int, Int)
quest03 = getInput 2026 3 part12 part12 part3

part12 :: String -> Int
part12 = sum . parseInput
  where
    parseInput = parse (p `P.sepBy` P.endOfLine)
      where
        p = uncurry calculateVote <$> pFormat "ISO"

part3 :: String -> Int
part3 = sum . parseInput
  where
    parseInput = catMaybes <$> parse (p `P.sepBy` P.endOfLine)
    p = do
      (i, j) <- pFormat "ISO2"
      let k = read j :: Int
      let v = calculateVote i j
      let r = find ((==v) . calculateVote i . show) [0..(k - 1)]
      pure $ if isNothing r then Just v else Nothing
    
calculateVote :: String -> String -> Int
calculateVote i j = sum $ zipWith (*) i' j'
  where
    i' = zipWith (^) (map digitToInt $ take (length j) $ cycle $ reverse i) [0..]
    j' = map digitToInt $ reverse j

pFormat :: String -> Parser (String, String)
pFormat s = do
  P.string s
  P.char '-'
  i <- P.many1 P.digit <* P.space
  j <- P.many1 P.digit
  pure (i, j)