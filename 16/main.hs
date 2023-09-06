dropEvery :: [a] -> Int -> [a]
dropEvery [] _ = []
dropEvery xs n = helper xs n
    where
        helper [] _ = []
        helper (x:xs) 1 = helper xs n
        helper (x:xs) c = x : helper xs (c - 1)

dropEvery' [] _ = []
dropEvery' l c = take (c - 1) l ++ dropEvery' (drop c l) c

