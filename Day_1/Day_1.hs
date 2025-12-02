main :: IO ()
main = do
    contents <- readFile "Day_1.txt"
    let linesOfFile = lines contents
    --print (turnDialPt1 0 50 linesOfFile)
    print (turnDialPt2 0 50 linesOfFile)

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
