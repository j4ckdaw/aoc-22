import Data.Array (Array, array, range, bounds, (!))

createGrid :: [String] -> Array (Int, Int) Int
createGrid input = grid
    where
        w = length (input !! 0) - 1
        h = length input - 1
        grid = array ((0,0), (h,w)) [((i,j), getTree (i,j)) | (i,j) <- range ((0,0), (h,w))]
        getTree (i,j) = read [(input !! i) !! j]::Int

visible :: Array (Int, Int) Int -> (Int, Int) -> Bool
visible grid (i,j) = up || down || left || right
    where
        (h,w) = snd $ bounds grid
        height = grid ! (i, j)
        up     = all (<height) [grid ! (k,j) | k <- [0..i-1]]
        down   = all (<height) [grid ! (k,j) | k <- [i+1..h]]
        left   = all (<height) [grid ! (i,k) | k <- [0..j-1]]
        right  = all (<height) [grid ! (i,k) | k <- [j+1..w]]

-- also takes the first element that fails the predicate (i.e the tree that blocks the view)
takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' _    []  = []
takeWhile' p (x:xs) = if p x then x : takeWhile' p xs else [x]

scenicScore :: Array (Int, Int) Int -> (Int, Int) -> Int
scenicScore grid (i,j) = up * down * left * right
    where
        (h,w) = snd $ bounds grid
        height = grid ! (i,j)
        up     = length $ takeWhile' (<height) $ reverse [grid ! (k,j) | k <- [0..i-1]]
        down   = length $ takeWhile' (<height)           [grid ! (k,j) | k <- [i+1..h]]
        left   = length $ takeWhile' (<height) $ reverse [grid ! (i,k) | k <- [0..j-1]]
        right  = length $ takeWhile' (<height)           [grid ! (i,k) | k <- [j+1..w]]


solveA :: [String] -> Int
solveA input = length $ filter id [vis ! (i,j) | (i,j) <- range $ bounds vis]
    where grid   = createGrid input
          vis    = array (bounds grid) [((i,j), visible grid (i,j)) | (i,j) <- range (bounds grid)]

solveB :: [String] -> Int
solveB input = maximum [scores ! (i,j) | (i,j) <- range $ bounds scores]
    where grid   = createGrid input
          scores = array (bounds grid) [((i,j), scenicScore grid (i,j)) | (i,j) <- range (bounds grid)]

main = do
    input <- readFile "inputs/8.txt"
    print $ solveA $ lines input
    print $ solveB $ lines input