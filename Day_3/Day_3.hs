import System.Environment (getArgs)
import Text.Read (readMaybe)
import Data.Char (digitToInt)
import Data.List (elemIndex)
import Data.Maybe (fromJust)

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

part2 :: Int -> [String] -> Int
part2 sum [] = sum
part2 sum (range:rest) =
    0