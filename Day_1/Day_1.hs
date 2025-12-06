import System.Environment (getArgs)
import Text.Read (readMaybe)

main :: IO ()
main = do
    args <- getArgs
    let part = case args of
            (p:_) -> maybe 1 id (readMaybe p :: Maybe Int)
            _     -> 1
        filename = case args of
            (_:f:_) -> f
            _       -> "Day_1.txt"
    contents <- readFile filename
    let linesOfFile = lines contents
    let output = case part of
            1 -> turnDialPt1 0 50 linesOfFile
            2 -> turnDialPt2 0 50 linesOfFile
            _ -> 0
    print(output)

turnDialPt1 count currPos [] = count
turnDialPt1 count currPos (line:rest) =
    let newPos = currPos `mod` 100
        newCount = if newPos == 0 then count + 1 else count
    in case line of
        (c:numStr) ->
            let num = read numStr :: Int
            in case c of
                'L' -> turnDialPt1 newCount (newPos - num) rest
                'R' -> turnDialPt1 newCount (newPos + num) rest
                _   -> turnDialPt1 newCount newPos rest
        _ -> turnDialPt1 newCount newPos rest

getZeroPasses nextPos num =
    (num `div` 100) + (if (nextPos > 99 || nextPos < 0) then 1 else 0)

turnDialPt2 count currPos [] = count
turnDialPt2 count currPos (line:rest) =
    let newPos = currPos `mod` 100
        --newCount = if newPos == 0 then count + 1 else count
    in case line of
        (c:numStr) ->
            let num = read numStr :: Int
            in case c of
                'L' -> turnDialPt2 (count + (getZeroPasses (newPos - num) num)) (newPos - num) rest
                'R' -> turnDialPt2 (count + (getZeroPasses (newPos + num) num)) (newPos + num) rest
                _   -> turnDialPt2 count newPos rest
        _ -> turnDialPt2 count newPos rest
