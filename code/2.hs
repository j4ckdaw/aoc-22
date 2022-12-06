import Data.List.Split (splitOn)

data Shape = Rock | Paper | Scissors
    deriving (Eq)
data Result = Lose | Draw | Win
    deriving (Eq)

shape :: String -> Shape
shape "A" = Rock
shape "B" = Paper
shape "C" = Scissors
shape "X" = Rock
shape "Y" = Paper
shape "Z" = Scissors

result :: String -> Result
result "X" = Lose
result "Y" = Draw
result "Z" = Win

shapeScore :: Shape -> Int
shapeScore Rock     = 1
shapeScore Paper    = 2
shapeScore Scissors = 3

resultScore :: Result -> Int
resultScore Lose = 0
resultScore Draw = 3
resultScore Win  = 6

pairA :: [String] -> (Shape, Shape)
pairA xs = (shape $ head xs, shape $ last xs)

pairB :: [String] -> (Shape, Result)
pairB xs = (shape $ head xs, result $ last xs)

playA :: (Shape, Shape) -> Result
playA (a, b)
    | a == b                       = Draw
    | a == Rock && b == Paper      = Win
    | a == Paper && b == Scissors  = Win
    | a == Scissors && b == Rock   = Win
    | otherwise                    = Lose

playB :: (Shape, Result) -> Shape
playB (a, b)
    | b == Draw                  = a
    | a == Rock     && b == Win  = Paper
    | a == Rock     && b == Lose = Scissors
    | a == Paper    && b == Win  = Scissors
    | a == Scissors && b == Lose = Paper
    | otherwise                  = Rock

scoreA :: (Shape, Shape) -> Int
scoreA (a, b) = (shapeScore b) + (resultScore $ playA (a, b))

scoreB :: (Shape, Result) -> Int
scoreB (a, b) = (resultScore b) + (shapeScore $ playB (a, b))

solveA :: String -> Int
solveA input = sum scores
    where
        games = map (pairA . words) (lines input)
        scores = map scoreA games

solveB :: String -> Int
solveB input = sum scores
    where
        games = map (pairB . words) (lines input)
        scores = map scoreB games

main = do
    input <- readFile "inputs/2.txt"
    print $ solveA input
    print $ solveB input
