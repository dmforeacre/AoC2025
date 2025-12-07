import System.Environment (getArgs)
import Text.Read (readMaybe)
import Data.List.Split (splitOn)

main :: IO ()
main = do
    args <- getArgs
    let part = case args of
            (p:_) -> maybe 1 id (readMaybe p :: Maybe Int)
            _     -> 1
        filename = case args of
            (_:f:_) -> f
            _       -> "Day_2.txt"
    contents <- readFile filename
    let ranges = splitOn "," contents
    let output = case part of
            1 -> part1 0 ranges
            2 -> part2 0 ranges
            _ -> 0
    print(output)

checkRange :: Int -> Int -> Int -> Int
checkRange curr end sum
    | curr > end   = sum
    | otherwise     = 
        let s = show curr
            len = length s
            (first, second) = splitAt (len `div` 2) s
        in if first == second then
            checkRange (curr + 1) end (sum + curr)
        else
            checkRange (curr + 1) end sum  
            
checkRepeats :: Int -> Int -> Int -> Int
checkRepeats curr end sum
    | curr > end   = sum
    | otherwise     = 
        let s = show curr
            hasDouble = any (\x -> length x >= 2) (map (\x -> filter (==x) s) ['0'..'9'])
            hasExactDouble = any (\x -> length x == 2) (map (\x -> filter (==x) s) ['0'..'9'])
            len = length s
            (first, second) = splitAt (len `div` 2) s
        in if first == second && hasExactDouble then
            checkRepeats (curr + 1) end (sum + curr)
        else
            checkRepeats (curr + 1) end sum

checkPattern :: String -> String -> Int
checkPattern curr num =
    | length curr > (length num) `div` 2 = 0
    | otherwise =
        
            
part1 :: Int -> [String] -> Int
part1 sum [] = sum
part1 sum (range:rest) = 
    let nums = splitOn "-" range
    in case nums of
        [startStr, endStr] ->
            case (readMaybe startStr :: Maybe Int, readMaybe endStr :: Maybe Int) of
                (Just start, Just end) -> part1 (sum + (checkRange start end 0)) rest
                _ -> 0
        _ -> 0

part2 :: Int -> [String] -> Int
part2 sum [] = sum
part2 sum (range:rest) =
    let nums = splitOn "-" range
    in case nums of
        [startStr, endStr] ->
            case (readMaybe startStr :: Maybe Int, readMaybe endStr :: Maybe Int) of
                (Just start, Just end) -> part2 (sum + (checkRepeats start end 0)) rest
                _ -> 0
        _ -> 0