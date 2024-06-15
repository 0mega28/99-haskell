elementAt :: [a] -> Int -> a
elementAt xs idx = xs !! (idx - 1)

elementAt' :: [a] -> Int -> a
elementAt' (x:_) 1 = x
elementAt' [] _ = error "index out of bounds"
elementAt' (_:xs) idx
    | idx < 1 = error "index out of bounds"
    | otherwise = elementAt' xs (idx - 1)
