{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Stories.S4.Quest01 where

import Common
import Control.Applicative ((<|>))
import Control.Lens
import Control.Monad.State.Strict
import Data.HashMap.Strict qualified as M
import Data.HashSet qualified as S
import Data.Tuple (swap)
import Debug.Trace
import ECSolution (getInput)
import Text.Parsec qualified as P
import Text.Parsec.String (Parser)

data Arcs = Arcs
  { _arcs :: (M.HashMap Int Int, M.HashMap Int Int),
    _visited :: S.HashSet Int,
    _visitedSeq :: [Int]
  }

data RightCheck = Valid | IncreaseTo Int | Impossible deriving (Show, Eq)

makeLenses ''Arcs

instance Show Arcs where
  show :: Arcs -> String
  show (Arcs arcs visited visitedSeq) =
    "above = "
      ++ show (fst arcs)
      ++ "\n"
      ++ "below = "
      ++ show (snd arcs)
      ++ "\n"
      ++ "visited = "
      ++ show visited
      ++ "\n"
      ++ "seq = "
      ++ show visitedSeq

initialArcs :: Arcs
initialArcs = Arcs (M.empty, M.empty) S.empty []

quest01 :: String -> IO (Int, Int, Int)
quest01 = getInput 4 1 part1 part2 part3

parseInput :: String -> [[Int]]
parseInput = map commaSeparatedInts . lines

part1 :: String -> Int
part1 = part12 True

part2 :: String -> Int
part2 = part12 False

part12 :: Bool -> String -> Int
part12 allowRepeats = sum . map getSequence . parseInput
  where
    getSequence :: [Int] -> Int
    getSequence xs = evalState (runSeq 0 xs) (S.singleton 0)
      where
        runSeq :: Int -> [Int] -> State (S.HashSet Int) Int
        runSeq v [] = return v
        runSeq v (x : xs) = do
          seen <- get

          vLeft <- runSeqLeft v x

          v' <- case vLeft of
            Nothing -> runSeqRight v x
            Just vLeft' -> pure vLeft'

          runSeq v' xs

        runSeqLeft :: Int -> Int -> State (S.HashSet Int) (Maybe Int)
        runSeqLeft v x = do
          seen <- get
          let vLeft = v - x
          if vLeft < 0 || vLeft `S.member` seen
            then do return Nothing
            else do
              modify $ S.insert vLeft
              return (Just vLeft)

        runSeqRight :: Int -> Int -> State (S.HashSet Int) Int
        runSeqRight v x = do
          seen <- get
          let vRight = v + x
          if not allowRepeats && vRight `S.member` seen
            then runSeqRight (succ v) x
            else do
              modify $ S.insert vRight
              return vRight

part3 :: String -> Int
part3 = sum . map getSequence . parseInput
  where
    getSequence :: [Int] -> Int
    getSequence xs = evalState (runSeq 0 xs) initialArcs

    runSeq :: Int -> [Int] -> State Arcs Int
    runSeq v [] = do
      modify $ over visited (S.insert v)
      modify $ over visitedSeq (v :)
      return v
    runSeq v (x : xs) = do
      modify $ over visited (S.insert v)
      modify $ over visitedSeq (v :)
      v'' <- runSeqLeft v x
      case v'' of
        Nothing -> runSeq v xs
        Just v' -> do
          modify $ over (arcs . _1) (M.insert v v' . M.insert v' v)
          modify $ over arcs swap
          runSeq v' xs

    checkLeft :: Int -> Int -> Int -> M.HashMap Int Int -> Maybe Int
    checkLeft start end i a =
      if i == end
        then Just end
        else case M.lookup i a of
          Nothing -> checkLeft start end (pred i) a
          Just v ->
            if v >= start
              then Nothing
              else
                if v <= end
                  then Nothing
                  else checkLeft start end (pred v) a

    runSeqLeft :: Int -> Int -> State Arcs (Maybe Int)
    runSeqLeft v x | v < x = runSeqRight v x
    runSeqLeft v x = do
      g <- get
      let vLeft = v - x

      if vLeft `S.member` (g ^. visited)
        then runSeqRight v x
        else do
          let a = fst (g ^. arcs)
          case checkLeft v vLeft (pred v) a of
            Nothing -> runSeqRight v x
            Just vl -> pure $ Just vl

    checkRight :: Int -> Int -> Int -> M.HashMap Int Int -> RightCheck
    checkRight start end i a
      | i == end = Valid
      | otherwise = case M.lookup i a of
          Nothing ->
            checkRight start end (succ i) a
          Just v
            | v <= start ->
                Impossible
            | v >= end ->
                IncreaseTo (succ v)
            | otherwise ->
                checkRight start end (succ v) a

    runSeqRight :: Int -> Int -> State Arcs (Maybe Int)
    runSeqRight v x = do
      g <- get

      let a = fst (g ^. arcs)
          vRight = v + x

      if vRight `S.member` (g ^. visited)
        then do
          case M.lookup vRight a of
            Nothing ->
              runSeqRight v (succ x)
            Just y
              | y < v ->
                  pure Nothing
              | y < vRight ->
                  runSeqRight v (succ x)
              | otherwise ->
                  runSeqRight v (y - v + 1)
        else do
          case checkRight v vRight (succ v) a of
            Valid ->
              pure (Just vRight)
            IncreaseTo next ->
              runSeqRight v (next - v)
            Impossible ->
              pure Nothing