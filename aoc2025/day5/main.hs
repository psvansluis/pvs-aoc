import Control.Monad (when)
import Data.Char (isDigit)
import Data.Function (on)
import Data.IntSet (fromDistinctAscList, fromList)
import Data.List (minimumBy, sortBy, sortOn)
import Data.Set (Set, empty, union)
import Data.Text (splitOn)
import Text.Parsec (sepBy)

main :: IO ()
main = do
  contents <- readFile "input.txt"
  let (ranges, ids) = parseFile contents
  print $ length $ filter (`isFresh` ranges) ids
  print $ countIdsInRange ranges
  print $ mergeRanges ranges

data Range = Range
  { start :: Int,
    endIncl :: Int
  }
  deriving (Show, Eq)

type Id = Int

parseFile :: String -> ([Range], [Id])
parseFile file = (ranges, ids)
  where
    (rangesRaw, idsRaw) = break null (lines file)
    ranges = map parseRange rangesRaw
    ids = map read (tail idsRaw)

parseRange :: String -> Range
parseRange line = Range start end
  where
    (startRaw, endRaw) = span isDigit line
    start = read startRaw
    end = read (tail endRaw)

isInRange :: Id -> Range -> Bool
isInRange id range = id >= start range && id <= endIncl range

isFresh :: Id -> [Range] -> Bool
isFresh id = any (isInRange id)

countIdsInRange :: [Range] -> Int
countIdsInRange ranges = length $ filter (`isFresh` ranges) [firstId .. lastId]
  where
    firstId = foldr (min . start) 0 ranges
    lastId = foldr (max . endIncl) 0 ranges

addRange :: Range -> [Range] -> [Range]
addRange r1 [] = [r1]
addRange r1@(Range s1 e1) (r2@(Range s2 e2) : rest) =
  if r1 `overlaps` r2
    then
      addRange (Range (min s1 s2) (max e1 e2)) rest
    else
      r1 : addRange r2 rest

overlaps :: Range -> Range -> Bool
overlaps (Range s1 e1) (Range s2 e2) = e1 >= s2

mergeRanges :: [Range] -> [Range]
mergeRanges ranges = foldr addRange [] sorted
  where
    sorted = sortOn start ranges