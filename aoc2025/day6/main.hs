import Data.List (transpose)

data MathProblem = MathProblem
  { operator :: Int -> Int -> Int,
    terms :: [Int],
    acc :: Int
  }

main :: IO ()
main = do
  contents <- readFile "input.txt"
  let parsed = parseFile contents
  let answers = map solveProblem parsed
  let answer = sum answers
  print answer

parseFile :: String -> [MathProblem]
parseFile file = map parseProblem t
  where
    ls = lines file
    ws = map words ls
    t = transpose ws

parseProblem :: [String] -> MathProblem
parseProblem column = MathProblem operator terms acc
  where
    (operator, acc) = case last column of
      "*" -> ((*), 1)
      "+" -> ((+), 0)
    terms = map (\x -> read x :: Int) (init column)

solveProblem :: MathProblem -> Int
solveProblem problem = foldr (operator problem) (acc problem) (terms problem)