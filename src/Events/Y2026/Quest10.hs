{-# LANGUAGE TemplateHaskell #-}

module Events.Y2026.Quest10 where

import Common
import Control.Lens
import Control.Monad (forM, forM_, when)
import Control.Monad.RWS.Strict
import Data.Function (on)
import Data.HashMap.Strict qualified as M
import Data.List (groupBy, intercalate, sortBy)
import Data.Maybe (mapMaybe)
import ECSolution (getInput)

type Walls = M.HashMap Point2d Char

type Ghosts = M.HashMap Point2d [(Char, Integer)]

data Ghost = Ghost
  { _ghostId :: Integer,
    _ghostPos :: Point2d,
    _ghostDir :: Char,
    _collisionCount :: Integer
  }

makeLenses ''Ghost

makeGhost :: Integer -> (Point2d, Char) -> Ghost
makeGhost gid (pos, dir) = Ghost gid pos dir 0

data Quest10State = Quest10State
  { _ghosts :: [Ghost],
    _collisionHistory :: M.HashMap String Integer,
    _ghostHistory :: M.HashMap Integer [Ghost]
  }

makeLenses ''Quest10State

createInitialState :: [Ghost] -> Quest10State
createInitialState gs =
  Quest10State
    { _ghosts = gs,
      _collisionHistory = M.empty,
      _ghostHistory = M.empty
    }

instance Show Ghost where
  show g = "Ghost " ++ show (g ^. ghostId) ++ ": p=" ++ show (g ^. ghostPos) ++ " d=" ++ [g ^. ghostDir] ++ " c=" ++ show (g ^. collisionCount)

instance Show Quest10State where
  show s =
    "Ghosts: "
      ++ gs
    where
      gs =
        unlines $
          [ show (g ^. ghostPos) ++ " " ++ show g
          | g <- s ^. ghosts
          ]

quest10 :: String -> IO (Integer, Integer, Integer)
quest10 = getInput 2026 10 part1 part2 part3

part1 :: String -> Integer
part1 i = sum $ map (^. collisionCount) $ s ^. ghosts
  where
    (gs, ws) = parseInput i
    (s, _) = execRWS go ws (createInitialState gs)
    go = forM_ [1 .. 10] $ \_ -> runRound

part2 :: String -> Integer
part2 = solve 10000 1 . parseInput

part3 :: String -> Integer
part3 = solve 1000000000000 1 . parseInput

parseInput :: String -> ([Ghost], Walls)
parseInput input = (ghosts, walls)
  where
    points = do
      (row, line) <- zip [0 ..] $ lines input
      (col, char) <- zip [0 ..] line
      pure ((row, col), char)
    ghosts = zipWith (makeGhost) [0 ..] $ filter ((`elem` "<>^v") . snd) points
    walls = M.fromList $ filter ((`elem` "#") . snd) points

solve :: Integer -> Integer -> ([Ghost], Walls) -> Integer
solve target round (gs, ws) = if null gs' then round' else solve target (round' + 1) (gs', ws)
  where
    (round', s, _) = runRWS (runTillDeletion target round) ws (createInitialState gs)
    gs' = s ^. ghosts

runTillDeletion :: Integer -> Integer -> RWS Walls () Quest10State Integer
runTillDeletion target rn = do
  mh <- checkHistory
  case mh of
    Just n -> do
      rn' <- skipRounds target n rn
      runTillDeletion target rn'
    Nothing -> do
      insertIntoHistory rn
      runRound

      anyDeletions <- checkDeletions target
      if anyDeletions
        then
          pure rn
        else
          runTillDeletion target (succ rn)

skipRounds :: Integer -> Integer -> Integer -> RWS Walls () Quest10State Integer
skipRounds t prn rn = do
  gn <- sortBy (compare `on` (^. ghostId)) <$> gets ((M.! prn) . (^. ghostHistory))
  g <- sortBy (compare `on` (^. ghostId)) <$> gets (^. ghosts)

  let dc =
        zipWith
          (-)
          (map (^. collisionCount) g)
          (map (^. collisionCount) gn)

      dn = rn - prn

      findN g d
        | d == 0 = Nothing
        | otherwise =
            Just $
              (t - (g ^. collisionCount) - 1) `div` d

      n = minimum $ mapMaybe (uncurry findN) (zip g dc)

      g' =
        zipWith
          (\g d -> over collisionCount (+ d * n) g)
          g
          dc

  modify $
    set collisionHistory M.empty
      . set ghostHistory M.empty
      . set ghosts g'

  pure $ rn + n * dn

insertIntoHistory :: Integer -> RWS Walls () Quest10State ()
insertIntoHistory rn = do
  s <- get
  k <- historyKey
  modify $ over collisionHistory (M.insert k rn) . over ghostHistory (M.insert rn (s ^. ghosts))

checkDeletions :: Integer -> RWS Walls () Quest10State Bool
checkDeletions target = do
  gs <- gets (^. ghosts)
  let gs' = filter ((< target) . (^. collisionCount)) gs

  if length gs > length gs'
    then (modify $ set ghosts gs') >> pure True
    else pure False

historyKey :: RWS Walls () Quest10State String
historyKey = do
  gs <- gets (^. ghosts)
  let gs' = map f $ sortBy (compare `on` (^. ghostId)) gs
  pure $ intercalate ";" gs'
  where
    f g = "id=" ++ show (g ^. ghostId) ++ ", pos=" ++ show (g ^. ghostPos) ++ ", dir=" ++ [g ^. ghostDir]

checkHistory :: RWS Walls () Quest10State (Maybe Integer)
checkHistory = do
  s <- get
  k <- historyKey
  pure $ M.lookup k (s ^. collisionHistory)

runRound :: RWS Walls () Quest10State ()
runRound = do
  gs <- gets (^. ghosts)

  gs'1 <- forM gs moveGhost
  gs'2 <- concat <$> forM (groupGhosts gs'1) ghostCollision
  modify $ set ghosts gs'2

  pure ()
  where
    moveGhost :: Ghost -> RWS Walls () Quest10State Ghost
    moveGhost g = do
      let p@(row, col) = g ^. ghostPos
          c = g ^. ghostDir
          n = g ^. collisionCount
      let (p', c') =
            case c of
              '>' -> ((row, col + 1), 'v')
              '<' -> ((row, col - 1), '^')
              '^' -> ((row - 1, col), '>')
              'v' -> ((row + 1, col), '<')
      r <- wallCollision p'
      pure
        if r
          then
            set ghostDir c' . over collisionCount succ $ g
          else
            set ghostPos p' g

    wallCollision :: Point2d -> RWS Walls () Quest10State Bool
    wallCollision p = asks (p `M.member`)

    ghostCollision :: [Ghost] -> RWS Walls () Quest10State [Ghost]
    ghostCollision [g] = pure [g]
    ghostCollision gs = pure $ map f gs
      where
        f = over ghostDir newDir . over collisionCount succ
        newDir = \case
          '>' -> '<'
          '<' -> '>'
          '^' -> 'v'
          'v' -> '^'

    groupGhosts :: [Ghost] -> [[Ghost]]
    groupGhosts = groupBy f . sortBy (compare `on` (^. ghostPos))
      where
        f g1 g2 = g1 ^. ghostPos == g2 ^. ghostPos
