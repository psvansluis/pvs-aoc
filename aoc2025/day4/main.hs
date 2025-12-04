import Data.List (transpose)
import Data.Maybe (catMaybes)
import Data.Sequence (mapWithIndex)
import Data.Tree (flatten)

main :: IO ()
main = do
  contents <- readFile "input.txt"
  let parsed = map (map readCell) $ lines contents
  let counted = countNeighbours parsed
  print $ countPickableRolls counted

countPickableRolls :: [[Cell]] -> Int
countPickableRolls cells = length pickableRolls
  where
    pickableRolls = filter isPickableRoll allCells
    isPickableRoll Empty = False
    isPickableRoll (Roll neighbors) = neighbors < 4
    allCells = concat cells

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