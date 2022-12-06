import Data.Set (fromList, size)

findMarker :: String -> Int -> Int -> Int
findMarker txt k i
    | size set == k = i + k
    | otherwise     = findMarker (tail txt) k (i + 1)
    where
        set = fromList $ take k txt

solveA :: String -> Int
solveA input = findMarker input 4 0

solveB :: String -> Int
solveB input = findMarker input 14 0

main = do
    input <- readFile "inputs/6.txt"
    print $ solveA input
    print $ solveB input