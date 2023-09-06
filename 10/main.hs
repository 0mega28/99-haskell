encode :: Eq a => [a] -> [(Int, a)]
encode [] = []
encode [c] = [(1, c)]
encode xs@(x:ys) = (length eq, x) : encode neq
    where
        eq = takeWhile (== x) xs
        neq = dropWhile (== x) xs
