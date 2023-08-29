myLength :: [a] -> Int
myLength = foldl (\a _ -> a + 1) 0
