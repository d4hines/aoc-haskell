module Day01 where
import Text.Read (readMaybe)
import Data.Maybe (catMaybes, fromMaybe)
import Data.List (sort)
import qualified Data.Map as Map

toMultiset :: [Int] -> Map.Map Int Int
toMultiset = foldr (\x acc -> Map.insertWith (+) x 1 acc) Map.empty

parseLine :: String -> Maybe (Int, Int)
parseLine line = case words line of
    [xStr, yStr] -> do
        x <- readMaybe xStr
        y <- readMaybe yStr
        return (x,y)
    _ -> Nothing

mainPart1 :: IO ()
mainPart1 = do
    contents <- readFile "./puzzles/01/input"
    let contentLines = lines contents
        parsed =  catMaybes $ map parseLine contentLines
        (left, right) = unzip parsed
        (leftSorted, rightSorted) = (sort left, sort right)
        zipped = zip leftSorted rightSorted
        differences = map (\(x,y) -> abs (x - y)) zipped
    putStrLn $ (show $ sum differences)

main :: IO ()
main = do
    contents <- readFile "./puzzles/01/input"
    let contentLines = lines contents
        (left, right) =  unzip $ catMaybes $ map parseLine contentLines
        rightMap = toMultiset right
        scores = map (\x -> fromMaybe 0 (fmap (x *) (Map.lookup x rightMap))) left
    putStrLn $ (show $ sum scores)

