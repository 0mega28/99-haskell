primes :: [Int]
primes = sieve [2..]
    where
        sieve (p:xs) = p : sieve [x | x <- xs , x `mod` p /= 0]

goldbach :: Int -> (Int, Int)
goldbach n = (x, n - x)
    where
        searchArray = takeWhile (<n) primes
        x = head [x | x <- searchArray, (n - x) `elem` searchArray ]


main = print $ goldbach 28