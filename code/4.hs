import Data.Set (Set, intersection, isSubsetOf, fromList, empty)
import Data.List.Split (splitOn)

makeSets :: String -> (Set Int, Set Int)
makeSets s = (makeSet $ head ranges, makeSet $ last ranges)
    where
        ranges          = map (splitOn "-") (splitOn "," s)
        makeSet [a, b]  = fromList [(read a::Int)..(read b::Int)]
        
solveA :: [String] -> Int
solveA    []  = 0
solveA (x:xs) = r + solveA xs
    where
        (a, b)  = makeSets x
        r       = if a `isSubsetOf` b || b `isSubsetOf` a then 1 else 0

solveB :: [String] -> Int
solveB    []  = 0
solveB (x:xs) = r + solveB xs
    where
        (a, b)  = makeSets x
        r       = if intersection a b /= empty then 1 else 0

main = do
    input <- readFile "inputs/4.txt"
    print $ solveA $ lines input
    print $ solveB $ lines input