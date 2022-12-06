import Data.List.Split (splitOn)
import Data.List (sort)

makeGroups :: String -> [[String]]
makeGroups text = splitOn [""] (lines text)

sumGroup :: [String] -> Int
sumGroup g = sum (map read g)

solveA :: String -> Int
solveA input = maximum totals
    where groups = makeGroups input
          totals = map sumGroup groups

solveB :: String -> Int
solveB input = sum (take 3 top)
    where groups = makeGroups input
          totals = map sumGroup groups
          top    = reverse $ sort totals

main = do
    input <- readFile "inputs/1.txt"
    print $ solveA input
    print $ solveB input
