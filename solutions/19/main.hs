rotate ls n
    | n == 0 = ls
    | n > 0 = drop n ls ++ take n ls
    | otherwise = reverse (rotate (reverse ls) (-n))
