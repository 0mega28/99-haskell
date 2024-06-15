myReverese :: [a] -> [a]
myReverese = foldl (\a x -> x : a) []