import Data.List (transpose)
import Data.Maybe (catMaybes)
import Data.Sequence (mapWithIndex)
import Data.Tree (flatten)

main :: IO ()
main = do
  contents <- readFile "input.txt"
  let parsed = map (map readCell) $ lines contents
  let counted = countNeighbours parsed
  print "part 1:"
  print (countPickableRollsPart1 counted)
  print "part 2:"
  print $ countPickableRollsPart2 parsed

countPickableRollsPart1 :: [[Cell]] -> Int
countPickableRollsPart1 board = length pickableRolls
  where
    pickableRolls = concatMap (filter isPickableRoll) board

countPickableRollsPart2 :: [[Cell]] -> Int
countPickableRollsPart2 board = countRolls board - countRolls endState
  where
    endState = until (\b -> b == iter b) iter board
    iter b = removePickableRolls $ countNeighbours b
    countRolls b = length $ concatMap (filter isRoll) b
    isRoll (Roll _) = True
    isRoll Empty = False

removePickableRolls :: [[Cell]] -> [[Cell]]
removePickableRolls = map (map removePickableRoll)

removePickableRoll :: Cell -> Cell
removePickableRoll Empty = Empty
removePickableRoll cell =
  if isPickableRoll cell
    then Empty
    else Roll 0

isPickableRoll :: Cell -> Bool
isPickableRoll Empty = False
isPickableRoll (Roll neighbors) = neighbors < 4

readCell :: Char -> Cell
readCell '@' = Roll 0
readCell '.' = Empty
readCell _ = undefined

increment :: Cell -> Cell
increment Empty = Empty
increment (Roll n) = Roll $ n + 1

countNeighbours :: [[Cell]] -> [[Cell]]
countNeighbours = doTimes 4 (countTopAndTopRight . rotl)

countTopAndTopRight :: [[Cell]] -> [[Cell]]
countTopAndTopRight board = case board of
  [] -> board
  [_] -> board
  (firstRow : secondRow : remainingRows) -> result
    where
      result = firstRow : countTopAndTopRight (secondRowAnnotated : remainingRows)
      secondRowAnnotated :: [Cell]
      secondRowAnnotated = zipWith annotateCell topAndTopRight secondRow
      topAndTopRight = zip firstRow (drop 1 firstRow ++ [Empty])
      annotateCell (Roll _, Roll _) cell = increment $ increment cell
      annotateCell (Empty, Roll _) cell = increment cell
      annotateCell (Roll _, Empty) cell = increment cell
      annotateCell (Empty, Empty) cell = cell

data Cell = Roll Int | Empty deriving (Show, Eq)

rotl :: [[a]] -> [[a]]
rotl = transpose . map reverse

doTimes :: Int -> (a -> a) -> a -> a
doTimes 0 f x = x
doTimes n f x = doTimes (n - 1) f (f x)