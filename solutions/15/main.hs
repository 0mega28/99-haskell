repli' :: [a] -> Int -> [a]
repli' xs n = foldr (\x a -> helper x n ++ a) [] xs
    where
        helper _ 0 = []
        helper x n = x:helper x (n - 1)