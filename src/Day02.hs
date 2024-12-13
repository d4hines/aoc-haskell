import Text.Read (readMaybe)
import Data.Maybe (catMaybes)

readMaybeInt :: String -> Maybe Int
readMaybeInt = readMaybe

parseLine :: String -> [Int]
parseLine line = catMaybes $ map readMaybeInt (words line)

diffInRange :: Int -> Int -> Bool
diffInRange x y = let diff = abs $ x - y in diff >= 1 && diff <= 3

cb :: [Int] -> Bool
cb [] = True
cb (_x : []) = True
cb (x : y : [] ) = diffInRange x y
cb (x : y : z : rest) =
    let diff = x - y
        diff' = y - z 
        monotonicSoFar = (diff >= 0 && diff' >= 0) || (diff < 0 && diff' < 0)
    in
        monotonicSoFar && diffInRange x y && cb (y : z : rest)

-- Part 1
-- main :: IO ()
-- main = do
--     contents <- readFile "./puzzles/02/input"
--     let l = [1,2,3]
--     putStrLn $ (show $ length $ filter id $ map (cb . parseLine) (lines contents))

removeEachElement :: [a] -> [[a]]
removeEachElement xs = [take i xs ++ drop (i + 1) xs | i <- [0..length xs - 1]]

main :: IO ()
main = do
    contents <- readFile "./puzzles/02/input"
    let l = map parseLine (lines contents)
    let all = map (\l' -> l' : removeEachElement l') l
    let matching = filter (any cb) all
    putStrLn $ show (length matching)

