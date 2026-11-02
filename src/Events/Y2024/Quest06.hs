module Events.Y2024.Quest06 where

import Common
import Control.Monad (forM_)
import Control.Monad.Writer
  ( MonadWriter (tell),
    Writer,
    execWriter,
  )
import Data.HashMap.Strict qualified as M
import ECSolution (getInput)
import Text.Parsec qualified as P
import Text.Parsec.String (Parser)

type Tree = M.HashMap String [String]

quest06 :: String -> IO (String, String, String)
quest06 = getInput 2024 6 p1 p2 p2
  where
    fn = solve <$> parse parseTree
    p1 = concat . fn
    p2 = map head . fn

parseTree :: Parser Tree
parseTree = M.fromList <$> (p `P.sepEndBy` P.newline)
  where
    p = do
      x <- P.letter `P.manyTill` P.char ':'
      y <- P.many1 (P.noneOf ",\n") `P.sepBy` P.char ','
      pure (x, y)

solve :: Tree -> [String]
solve m = concat $ head $ M.elems $ M.filter ((== 1) . length) r'
  where
    r = execWriter (go [] "RR")
    r' = foldr1 (M.unionWith (++)) $ zipWith M.singleton (map length r) (map (: []) r)
    go :: [String] -> String -> Writer [[String]] ()
    go s = \case
      "@" -> tell [reverse $ "@" : s]
      "ANT" -> pure ()
      "BUG" -> pure ()
      k -> forM_ (M.lookupDefault [] k m) (go (k : s))
