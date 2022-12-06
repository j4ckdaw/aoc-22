import Data.List (splitAt)
import Data.Char (isSpace)

parseState :: [String] -> [String]
parseState text = map parseColumn [0..8]
    where
        parseColumn i = dropWhile isSpace $ map (getChar i) (take 8 text)
        getChar i row = row !! (4 * i + 1)

parseMove :: String -> (Int, Int, Int)
parseMove line = ((read a::Int), (read b::Int) - 1, (read c::Int) - 1)
    where
        ws        = words line
        (a, b, c) = (ws !! 1, ws !! 3, ws !! 5)

doMove :: (String -> String) -> [String] -> (Int, Int, Int) -> [String]
doMove f state (n, a, b) = map change (zip [0..] state) 
    where
        items  = take n (state !! a)
        change (i, e)
            | i == a    = drop n e
            | i == b    = (f items) ++ e
            | otherwise = e

doMoves :: (String -> String) -> [String] -> [(Int, Int, Int)] -> [String]
doMoves f state    []  = state
doMoves f state (m:ms) = doMoves f (doMove f state m) ms

solveA :: [String] -> String
solveA input = map head res
    where
        moves = map parseMove (drop 10 input)
        res   = doMoves reverse (parseState input) moves

solveB :: [String] -> String
solveB input = map head res
    where
        moves = map parseMove (drop 10 input)
        res   = doMoves id (parseState input) moves

main = do
    input <- readFile "inputs/5.txt"
    print $ solveA $ lines input
    print $ solveB $ lines input