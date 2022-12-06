import Data.Set (Set, intersection, fromList, elemAt)
import Data.Char (ord)

priority :: Char -> Int
priority c 
    | n < 0     = n + 58
    | otherwise = n
    where n = (ord c - 96)

commonItem :: (Set Char, Set Char) -> Char
commonItem (a, b) = elemAt 0 $ intersection a b

rucksacksA :: String -> [(Set Char, Set Char)]
rucksacksA text = map makeSets (map split (lines text))
    where split s         = splitAt ((length s) `div` 2) s
          makeSets (a, b) = (fromList a, fromList b)

solveA :: String -> Int
solveA input = sum $ map priority (map commonItem (rucksacksA input))

rucksacksB :: String -> [Set Char]
rucksacksB text = map fromList (lines text)

badges :: [Set Char] -> [Char]
badges [] = []
badges rs = (badges' $ take 3 rs) : (badges $ drop 3 rs)
    where
        badges' [a,b,c] = elemAt 0 $ intersection a $ intersection b c 

solveB :: String -> Int
solveB input = sum $ map priority (badges $ rucksacksB input)

main = do
    input <- readFile "inputs/3.txt"
    print $ solveA input
    print $ solveB input