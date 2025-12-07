import System.Environment (getArgs)
import Text.Read (readMaybe)
import Debug.Trace (traceShow)

main :: IO ()
main = do
    args <- getArgs
    let part = case args of
            (p:_) -> maybe 1 id (readMaybe p :: Maybe Int)
            _     -> 1
        filename = case args of
            (_:f:_) -> f
            _       -> "Day_1test.txt"
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
            case readMaybe numStr :: Maybe Int of
                Just num ->
                    case c of
                        'L' -> turnDialPt1 newCount (newPos - num) rest
                        'R' -> turnDialPt1 newCount (newPos + num) rest
                        _   -> 0
                Nothing -> 0
        _ -> 0

turnDialPt2 count currPos [] = count
turnDialPt2 count currPos (line:rest) =
    let newPos = currPos `mod` 100
        newCount = if newPos == 0 then count + 1 else count
    in case line of
        (c:numStr) ->
            case readMaybe numStr :: Maybe Int of
                Just num ->
                    let dbg = traceShow (newPos, newCount, line, newPos - (num `mod` 100), newPos + (num `mod` 100)) ()
                    in dbg `seq` case c of
                        'L' -> turnDialPt2 (newCount + (num `div` 100) + (if newPos - (num `mod` 100) < 0 && newPos /= 0 then 1 else 0)) (newPos - num) rest
                        'R' -> turnDialPt2 (newCount + (num `div` 100) + (if newPos + (num `mod` 100) > 100 && newPos /= 0 then 1 else 0)) (newPos + num) rest
                        _   -> 0
                Nothing -> 0
        _ -> 0