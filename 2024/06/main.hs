import Data.List (elemIndex, find, findIndex, intercalate, transpose)
import Data.Maybe (fromJust, fromMaybe)
import System.Directory.Internal.Prelude (getArgs)
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
showMap m = intercalate "\n" $ map (concatMap show) m

parse :: Char -> Position
parse '.' = Space {visited = False}
parse '^' = Guard
parse '#' = Obstacle
parse _ = undefined

stringToPositions :: String -> Map
stringToPositions str = rotateRight $ map (map parse) (lines str)

hasGuard :: [Position] -> Bool
hasGuard lst = Guard `elem` lst

rotateLeft :: [[a]] -> [[a]]
rotateLeft = reverse . transpose

rotateRight :: [[a]] -> [[a]]
rotateRight = rotateLeft . rotateLeft . rotateLeft

canAdvance :: Map -> Bool
canAdvance = any hasGuard

advance :: Map -> Map
advance m = rotateLeft updateGuardPos
  where
    updateGuardPos = map (\r -> if hasGuard r then moveGuard r else r) m

moveGuard :: [Position] -> [Position]
moveGuard ps = pre ++ visitedSpaces ++ post
  where
    (pre, afterPre) = span (/= Guard) ps
    (mid, post) = span (/= Obstacle) afterPre
    visitedSpaces =
      if null post
        then replicate (length mid) Space {visited = True}
        else replicate (length mid - 1) Space {visited = True} ++ [Guard]

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
  args <- getArgs
  fileHandle <- openFile (head args) ReadMode
  contents <- hGetContents fileHandle
  let map = stringToPositions contents
  print $ part1Answer map
  hClose fileHandle
