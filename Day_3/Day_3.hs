import System.Environment (getArgs)
import Text.Read (readMaybe)
import Data.Char (digitToInt)
import Data.List (elemIndex)
import Data.Maybe (fromJust)
import Data.List (foldl')
import Debug.Trace (traceShow)

main :: IO ()
main = do
    args <- getArgs
    let part = case args of
            (p:_) -> maybe 1 id (readMaybe p :: Maybe Int)
            _     -> 1
        filename = case args of
            (_:f:_) -> f
            _       -> "Day_3.txt"
    contents <- readFile filename
    let linesOfFile = lines contents
    let output = case part of
            1 -> part1 0 linesOfFile
            2 -> part2 0 linesOfFile
            _ -> 0
    print(output)    

part1 :: Int -> [String] -> Int
part1 sum [] = sum
part1 sum (bank:rest) = 
    let joltages = map digitToInt bank
        max = maximum (init joltages)
        index = elemIndex max (init joltages)
    in  part1 (sum + (max * 10) + (maximum (drop (fromJust index +1) joltages))) rest

getTwelve :: Int -> Int -> [Int] -> [Int] -> [Int]
getTwelve count len joltages bank =
    if count == -1 then bank
    else
        let curr = take (len - count) joltages
            max = maximum curr
            index = elemIndex max curr
            nextJoltages = (drop (fromJust index + 1) joltages)
        in getTwelve (count - 1) (length nextJoltages) nextJoltages (bank ++ [max]) 

part2 :: Int -> [String] -> Int
part2 sum [] = sum
part2 sum (bank:rest) =
    let joltages = map digitToInt bank
        twelve = getTwelve 11 (length joltages) joltages []
        in part2 (sum + (foldl' (\acc x -> acc * 10 + x) 0 twelve)) rest