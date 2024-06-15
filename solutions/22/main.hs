range :: Int -> Int -> [Int]
range s e
    | s < e = (s:range (s + 1) e)
    | s == e = [e]
    | otherwise = error "start index should be less or equal to end index"