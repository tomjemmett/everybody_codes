module Events.Y2026.Quest07 where

import Common
import Control.Monad (forM)
import Control.Monad.RWS.Strict
import Data.Bifunctor (bimap, second)
import Data.Function (on)
import Data.HashMap.Strict qualified as M
import Data.HashSet qualified as S
import Data.List (maximumBy)
import ECSolution (getInput)
import Text.Parsec qualified as P
import Text.Parsec.String (Parser)

type CloudState = RWS (Int, M.HashMap Int (Int, [Int])) () (S.HashSet Int, M.HashMap Int Int)

quest07 :: String -> IO (Int, Int, Int)
quest07 = getInput 2026 7 part1 part2 part3

part1 :: String -> Int
part1 = part12 5

part2 :: String -> Int
part2 = part12 10

part12 :: Int -> String -> Int
part12 impulse input = run target i impulse init
  where
    i = parseInput input
    target = 10000000 -- large value so no clouds pop
    init = fst $ maximumBy (compare `on` snd) $ M.toList $ M.map (length . snd) i

part3 :: String -> Int
part3 input = maximum $ map (run target i 10) $ M.keys i
  where
    i = parseInput input
    target = if length (M.keys i) == 7 then 100 else 1000000

run :: Int -> M.HashMap Int (Int, [Int]) -> Int -> Int -> Int
run target i impulse init = sum $ M.elems $ snd s
  where
    r = (target, i)
    (s, _) = execRWS (go [(init, (impulse, 1))]) r (S.empty, M.map fst i)

go :: [(Int, (Int, Int))] -> CloudState ()
go [] = pure ()
go xs = do
  xs' <- getNextXs <$> sendImpulses xs <*> popClouds xs
  go =<< xs'

sendImpulses :: [(Int, (Int, Int))] -> CloudState [(Int, (Int, Int))]
sendImpulses xs = do
  (_, i) <- ask
  (d, _) <- get
  xs' <- forM xs $ \(x, (strength, count)) -> do
    if strength == 0 || S.member x d
      then pure []
      else do
        modify $ second (M.insertWith (+) x (strength * count))
        let connections = snd $ i M.! x
        pure $ map (,(pred strength, count)) connections
  pure $ concat xs'

popClouds :: [(Int, (Int, Int))] -> CloudState [(Int, (Int, Int))]
popClouds xs = do
  (target, i) <- ask
  xs' <- forM xs $ \(x, _) -> do
    f <- fmap (M.lookup x . snd) get
    if f >= Just target
      then do
        modify $ bimap (S.insert x) (M.delete x)
        let connections = snd $ i M.! x
        pure $ map (,(10, 1)) connections
      else pure []
  pure $ concat xs'

-- combine the results from sendImpulses and popClouds, combine any case where
-- there is a cloud with multiple entries with the same count, and remove any
-- clouds that have popped
getNextXs :: [(Int, (Int, Int))] -> [(Int, (Int, Int))] -> CloudState [(Int, (Int, Int))]
getNextXs xs ys = do
  d <- fst <$> get
  pure (g . M.toList . M.fromListWith (+) . f $ filter (\(x, _) -> not (S.member x d)) $ xs ++ ys)
  where
    g = map (\((x, s), c) -> (x, (s, c)))
    f = map (\(x, (s, c)) -> ((x, s), c))

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