data Item    = Dir String [Item] | File String Int
    deriving (Show)
data Command = List | Root | Up | Enter String
    deriving (Eq)

isItem :: String -> Bool
isItem s = (head s) /= '$'

parseCommand :: String -> Command
parseCommand cmd
    | args !! 1 == "ls" = List
    | args !! 2 == "/"  = Root
    | args !! 2 == ".." = Up
    | otherwise         = Enter (args !! 2)
    where
        args = words cmd

makeDir :: [String] -> [Item]
makeDir items = map (makeItem) items
    where
        makeItem :: String -> Item
        makeItem i
            | prefix == "dir" = Dir name []
            | otherwise       = File name (read prefix::Int)
            where
                [prefix, name] = words i

findDir :: String -> [Item] -> (Item, [Item])
findDir name items = (i, is)
    where
        nameMatches :: String -> Item -> Bool
        nameMatches a (Dir b _)  = a == b
        nameMatches a (File _ _) = False
        i  = (filter (nameMatches name) items) !! 0
        is = filter (not . nameMatches name) items 

mutateFilesystem :: [String] -> Item -> [String] -> Item
mutateFilesystem cs (Dir n fs)    []  = Dir n (makeDir cs)
mutateFilesystem cs (Dir n fs) (p:ps) = Dir n fs'
    where
        (target, rest) = findDir p fs
        f' = mutateFilesystem cs target ps
        fs' = (f':rest)

-- Inputs: Commands, Root dir, Working dir path
makeFilesystem :: [String] -> Item -> [String] -> Item
makeFilesystem [] i _ = i
makeFilesystem (c:cs) fs path
    | cmd == Up    = makeFilesystem cs fs (init path)
    | cmd == Root  = makeFilesystem cs fs []
    | cmd == List  = makeFilesystem (dropWhile isItem cs) (mutateFilesystem (takeWhile isItem cs) fs path) path
    | otherwise    = makeFilesystem cs fs (path ++ [target])
    where 
        cmd = parseCommand c
        (Enter target) = cmd

calcSizes :: Item -> (Int, [Int])
calcSizes (File _ s) = (s, [])
calcSizes (Dir _ is) = (s, s:subSizes)
    where
        sizes = map calcSizes is
        subSizes = concat $ map snd sizes
        s = sum $ map fst sizes
        
solveA :: [String] -> Int
solveA input = sum $ filter (<100000) targets
    where
        fs = makeFilesystem (drop 1 input) (Dir "/" []) []
        targets = snd $ calcSizes fs 

solveB :: [String] -> Int
solveB input = minimum $ filter (>spaceToFree) targets
    where
        fs = makeFilesystem (drop 1 input) (Dir "/" []) []
        (totalSize, targets) = calcSizes fs
        spaceToFree = totalSize - 40000000

main = do
    input <- readFile "inputs/7.txt"
    print $ solveA $ lines input
    print $ solveB $ lines input