module Events.Y2026.Quest06 where

import Common
import Control.Monad.State
import Data.HashMap.Strict qualified as M
import Data.HashSet qualified as S
import ECSolution (getInput)
import Text.Parsec qualified as P
import Text.Parsec.String (Parser)

quest06 :: String -> IO (Int, Int, Int)
quest06 = getInput 2026 6 part1 part2 part3

part1 :: String -> Int
part1 = S.size . search "NULL" . parseInput
  where
    search :: String -> M.HashMap String (S.HashSet String) -> S.HashSet String
    search item m = S.delete item (go [item] S.empty)
      where
        go :: [String] -> S.HashSet String -> S.HashSet String
        go [] acc = acc
        go (x : xs) acc =
          if x `S.member` acc
            then go xs acc
            else case M.lookup x m of
              Nothing -> go xs acc'
              Just ingredients -> go (S.toList ingredients ++ xs) acc'
          where
            acc' = S.insert x acc

part2 :: String -> Int
part2 = S.size . search "NULL" . parseInput

part3 :: String -> Int
part3 = M.size . p3 . parseInput
  where
    p3 i =
      M.filter (> 1) $
        M.fromListWith (+) $
          concat [map (,1) $ S.toList $ search ("NULLv" ++ show x) i | x <- [1 .. 3]]

parseInput :: String -> M.HashMap String (S.HashSet String)
parseInput = M.fromList . parse (p `P.sepEndBy` P.newline)
  where
    p :: Parser (String, S.HashSet String)
    p = do
      item <- P.manyTill P.alphaNum (P.char ':')
      ingredients <- P.many P.alphaNum `P.sepBy` P.char ','
      pure (item, S.fromList ingredients)

search :: String -> M.HashMap String (S.HashSet String) -> S.HashSet String
search item m = S.delete item (go [item] S.empty)
  where
    go [] acc = acc
    go (x : xs) acc =
      if x `S.member` acc
        then go xs acc
        else go (nx ++ xs) acc'
      where
        nx = M.keys $ M.filter (S.member x) m
        acc' = S.insert x acc
