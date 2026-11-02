module Stories.S3.Quest01 where

import Common
import Data.Char (isUpper)
import Data.Function (on)
import Data.HashMap.Strict qualified as M
import Data.List (maximum, maximumBy, minimumBy)
import Data.Maybe (mapMaybe)
import ECSolution (getInput)
import Text.Parsec qualified as P

data Scale = Scale
  { scaleId :: Int,
    values :: [Int]
  }
  deriving (Show)

quest01 :: String -> IO (Int, Int, Int)
quest01 = getInput 3 1 part1 part2 part3

part1 :: String -> Int
part1 = sum . map scaleId . filter greenIsDominant . parseInput
  where
    greenIsDominant (Scale {..}) = green > max red blue
      where
        [red, green, blue] = values

part2 :: String -> Int
part2 input = scaleId $ minimumBy (compare `on` colourSum) shiniest
  where
    i = parseInput input
    maxShine = maximum $ map (last . values) i
    shiniest = filter ((== maxShine) . last . values) i
    colourSum = sum . init . values

part3 :: String -> Int
part3 =
  sum
    . maximumBy (compare `on` length)
    . M.elems
    . M.fromListWith (++)
    . mapMaybe getGroup
    . parseInput
  where
    getGroup (Scale {..}) = case (++) <$> finish <*> dominant of
      Nothing -> Nothing
      Just k -> Just (k, [scaleId])
      where
        [red, green, blue, shine] = values
        finish
          | shine <= 30 = Just "matte"
          | shine >= 33 = Just "shiny"
          | otherwise = Nothing
        dominant
          | red > max green blue = Just "red"
          | green > max red blue = Just "green"
          | blue > max red green = Just "blue"
          | otherwise = Nothing

parseInput :: String -> [Scale]
parseInput = parse (p `P.sepBy` P.newline)
  where
    p = do
      id <- number <* P.char ':'
      values <- map convert <$> P.many P.letter `P.sepBy` P.char ' '
      pure $ Scale id values
    fn = fromEnum . isUpper
    convert = foldl (\acc c -> acc * 2 + fn c) 0
