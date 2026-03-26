{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}

module Stories.S3.Quest03 where

import Common
import Control.Lens
import Data.Maybe (fromMaybe)
import ECSolution (getInput)
import Text.Parsec qualified as P
import Text.Parsec.String (Parser)

data Node = Node
  { _nodeId :: Int,
    _plug :: (String, String),
    _leftSocket :: (String, String),
    _rightSocket :: (String, String),
    _leftNode :: Maybe (Bool, Node),
    _rightNode :: Maybe (Bool, Node)
  }
  deriving (Eq)

data Tree where
  Tree :: {_rootNode :: Node} -> Tree
  deriving (Eq)

makeLenses ''Node

instance Show Node where
  show = treeToString 0 '-'

instance Show Tree where
  show (Tree n) = show n

type MakeConnection = (String, String) -> (String, String) -> Maybe Bool

treeToString :: Int -> Char -> Node -> [Char]
treeToString i j n = do
  let spaces = replicate i ' '
      s1 =
        concat
          [ spaces,
            j : ": ",
            show (n ^. nodeId),
            " plug: ",
            show (n ^. plug),
            " leftSocket: ",
            show (n ^. leftSocket),
            " rightSocket: ",
            show (n ^. rightSocket)
          ]
      sl = case n ^. leftNode of
        Nothing -> spaces ++ "  l: None"
        Just (b, n) -> treeToString (i + 2) (if b then 'L' else 'l') n
      sr = case n ^. rightNode of
        Nothing -> spaces ++ "  r: None"
        Just (b, n) -> treeToString (i + 2) (if b then 'R' else 'r') n
  s1 ++ "\n" ++ sl ++ "\n" ++ sr

quest03 :: String -> IO (Int, Int, Int)
quest03 = getInput 3 3 (solve part1) (solve part2) (solve part3)

solve :: MakeConnection -> String -> Int
solve m = getChecksum . makeTree m . parseInput
  where
    getChecksum :: Tree -> Int
    getChecksum (Tree n) = fst $ go (Just n) (0, 1)
      where
        go :: Maybe Node -> (Int, Int) -> (Int, Int)
        go Nothing vi = vi
        go (Just t) vi = vr
          where
            (v, i) = go (t ^. leftNode ^? _Just . _2) vi
            vt = (v + (t ^. nodeId) * i, succ i)
            vr = go (t ^. rightNode ^? _Just . _2) vt

part1 :: (String, String) -> (String, String) -> Maybe Bool
part1 (a, b) (c, d)
  | a == c && b == d = Just True
  | otherwise = Nothing

part2 :: (String, String) -> (String, String) -> Maybe Bool
part2 (a, b) (c, d)
  | a == c && b == d = Just True
  | a == c || b == d = Just True
  | otherwise = Nothing

part3 :: (String, String) -> (String, String) -> Maybe Bool
part3 (a, b) (c, d)
  | a == c && b == d = Just True
  | a == c || b == d = Just False
  | otherwise = Nothing

-- Parsing
makeNode :: Int -> (String, String) -> (String, String) -> (String, String) -> Node
makeNode i p ls rs = Node i p ls rs Nothing Nothing

parseInput :: String -> [Node]
parseInput = parse (parseNode `P.sepBy` P.newline)

parseNode :: Parser Node
parseNode = do
  i <- P.string "id=" *> number
  p <- P.string ", plug=" *> pPair
  ls <- P.string ", leftSocket=" *> pPair
  rs <- P.string ", rightSocket=" *> pPair
  P.many $ P.noneOf "\n"
  return $ makeNode i p ls rs
  where
    pPair = (,) <$> (P.many P.letter <* P.space) <*> P.many P.letter

-- solving
makeTree :: MakeConnection -> [Node] -> Tree
makeTree makeConnection (n : ns) = foldl (addNodeToTree makeConnection) (Tree n) ns

addNodeToTree :: MakeConnection -> Tree -> Node -> Tree
addNodeToTree makeConnection (Tree root) n = Tree $ go root n
  where
    go t n = case tryInsertLeft t n of
      (t', Nothing) -> t'
      (t', Just n') -> go t' n'
    tryInsertLeft :: Node -> Node -> (Node, Maybe Node)
    tryInsertLeft t n = do
      let con = makeConnection (t ^. leftSocket) (n ^. plug)
      case t ^. leftNode of
        Nothing -> case con of
          Nothing -> tryInsertRight t n
          Just b -> (t & leftNode ?~ (b, n), Nothing)
        Just (b, ln) -> do
          let c = fromMaybe False con
          if b < c
            then
              tryInsertRight (t & leftNode ?~ (c, n)) ln
            else
              let (t', n') = tryInsertLeft ln n
               in case n' of
                    Nothing -> (t & leftNode ?~ (b, t'), Nothing)
                    Just n'' -> tryInsertRight (t & leftNode ?~ (b, t')) n''
    tryInsertRight :: Node -> Node -> (Node, Maybe Node)
    tryInsertRight t n = do
      let con = makeConnection (t ^. rightSocket) (n ^. plug)
      case t ^. rightNode of
        Nothing -> case con of
          Nothing -> (t, Just n)
          Just b -> (t & rightNode ?~ (b, n), Nothing)
        Just (b, rn) -> do
          let c = fromMaybe False con
          if b < c
            then (t & rightNode ?~ (c, n), Just rn)
            else
              let (t', n') = tryInsertLeft rn n
               in (t & rightNode ?~ (b, t'), n')
