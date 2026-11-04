module Events.Y2026.Quest07 where

import Common
import Control.Monad (forM, guard)
import Control.Monad.State.Strict
import Data.Bifunctor (bimap, second)
import Data.Function (on)
import Data.HashMap.Strict qualified as M
import Data.HashSet qualified as S
import Data.List (maximumBy)
import Debug.Trace
import ECSolution (getInput)
import Text.Parsec qualified as P
import Text.Parsec.String (Parser)

quest07 :: String -> IO (Int, Int, Int)
quest07 = getInput 2026 7 part1 part2 part3

part1 :: String -> Int
part1 input = sum $ M.elems $ snd $ execState (go 10000000 i [(init, (5, 1))]) (S.empty, M.map fst i)
  where
    i = parseInput input
    init = fst $ maximumBy (compare `on` snd) $ M.toList $ M.map (length . snd) i

part2 :: String -> Int
part2 input = sum $ M.elems $ snd $ execState (go 10000000 i [(init, (10, 1))]) (S.empty, M.map fst i)
  where
    i = parseInput input
    init = fst $ maximumBy (compare `on` snd) $ M.toList $ M.map (length . snd) i

part3 :: String -> Int
part3 input = maximum $ map run $ M.keys i
  where
    i = parseInput input
    target = if length (M.keys i) == 7 then 100 else 1000000
    run init = sum $ M.elems $ snd $ execState (go target i [(init, (10, 1))]) (S.empty, M.map fst i)

go :: Int -> M.HashMap Int (Int, [Int]) -> [(Int, (Int, Int))] -> State (S.HashSet Int, M.HashMap Int Int) ()
go _ _ [] = pure ()
go target i xs = do
  (d, s) <- get
  xs' <- forM xs $ \(x, (strength, count)) -> do
    if strength == 0 || S.member x d
      then pure []
      else do
        modify $ second (M.insertWith (+) x (strength * count))
        let connections = snd $ i M.! x
        pure $ map (,(pred strength, count)) connections
  xs'' <- forM xs $ \(x, _) -> do
    f <- fmap (M.lookup x . snd) get
    if f >= Just target
      then do
        modify $ bimap (S.insert x) (M.delete x)
        let connections = snd $ i M.! x
        pure $ map (,(10, 1)) connections
      else pure []
  d' <- fmap fst get
  go target i $
    map (\((x, s), c) -> (x, (s, c))) $
      M.toList $
        M.fromListWith (+) $
          map (\(x, (s, c)) -> ((x, s), c)) $
            filter (\(x, _) -> not (S.member x d')) $
              concat xs' ++ concat xs''

parseInput :: String -> M.HashMap Int (Int, [Int])
parseInput = M.fromList . parse (p `P.sepEndBy` P.newline)
  where
    p = do
      id <- P.string "id=" *> number
      energy <- P.string ", energy=" *> number
      connections <- P.string ", connections=" *> (number `P.sepBy` P.char ',')
      pure (id, (energy, connections))
    number :: Parser Int
    number = read <$> P.many1 P.digit