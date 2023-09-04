compress :: Eq a => [a] -> [a]
compress [] = []
compress [a] = [a]
compress (a:b:xs) = if a == b then compress (a:xs) else a : compress (b:xs)
