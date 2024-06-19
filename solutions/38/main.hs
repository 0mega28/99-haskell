primes :: [Int]
primes = sieve [2..]
    where
        sieve (p:xs) = p : sieve [x | x <- xs , x `mod` p /= 0]

primesR a b = takeWhile (<= b) $ dropWhile (<a) primes

main = do print $ primesR 11 19
