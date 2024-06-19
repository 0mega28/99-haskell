primes :: [Int]
primes = sieve [2..]
    where
        sieve (p:xs) = p : sieve [x | x <- xs , x `mod` p /= 0]

goldbach :: Int -> (Int, Int)
goldbach n = (x, n - x)
    where
        searchArray = takeWhile (<n) primes
        x = head [x | x <- searchArray, (n - x) `elem` searchArray ]

goldbach' n = head [(x, y) | x <- pr, y <- pr, x + y == n]
    where 
        pr = takeWhile (<n) primes

main = do
    print $ goldbach 28
    print $ goldbach' 28