import Data.List (elemIndex, findIndex, intercalate, transpose)
import Data.Maybe (fromJust, fromMaybe)
import System.IO
  ( IOMode (ReadMode),
    hClose,
    hGetContents,
    openFile,
  )

data Direction = North | East | South | West

data Position
  = Guard
  | Space {visited :: Bool}
  | Obstacle
  deriving (Eq)

instance Show Position where
  show :: Position -> String
  show Guard = "^"
  show (Space v) = if v then "X" else "."
  show Obstacle = "#"

type Map = [[Position]]

showMap :: Map -> String
showMap m = intercalate "\n" $ map showLine m

showLine :: [Position] -> String
showLine = concatMap Prelude.show

parse :: Char -> Position
parse '.' = Space {visited = False}
parse '^' = Guard
parse '#' = Obstacle
parse _ = undefined

stringToPositions :: String -> Map
stringToPositions str =
  let ls = lines str
      parseLine = map parse
   in map parseLine ls

hasGuard :: [Position] -> Bool
hasGuard lst = Guard `elem` lst

rotateLeft :: [[a]] -> [[a]]
rotateLeft = reverse . transpose

canAdvance :: Map -> Bool
canAdvance m = not ((hasGuard . head) m)

advance :: Map -> Map
advance map =
  if mustTurn
    then turnRight
    else (forwardToGuard . guardToVisited) map
  where
    guardRowI = fromJust $ findIndex hasGuard map
    guardColI = fromJust $ elemIndex Guard $ map !! guardRowI
    mustTurn = (map !! (guardRowI - 1) !! guardColI) == Obstacle
    guardToVisited = replace guardRowI guardColI Space {visited = True}
    forwardToGuard = replace (guardRowI - 1) guardColI Guard
    turnRight = rotateLeft map

replace :: Int -> Int -> Position -> Map -> Map
replace rowI colI newPosition map = replace' rowI newRow map
  where
    replace' i newVal list = take i list ++ newVal : drop (i + 1) list
    newRow = replace' colI newPosition (map !! rowI)

countVisited :: Map -> Int
countVisited m = (length . filter isVisited) (concat m)
  where
    isVisited Guard = True
    isVisited (Space v) = v
    isVisited _ = False

part1Answer :: Map -> Int
part1Answer m = countVisited guardAtEnd
  where
    guardAtEnd = until (not . canAdvance) advance m

main :: IO ()
main = do
  fileHandle <- openFile "realData.txt" ReadMode
  contents <- hGetContents fileHandle
  let map = stringToPositions contents
  print $ part1Answer map
  hClose fileHandle
