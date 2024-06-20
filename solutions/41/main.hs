primes :: [Int]
primes = sieve [2..]
    where
        sieve (p:xs) = p : sieve [x | x <- xs , x `mod` p /= 0]

goldbach :: Int -> (Int, Int)
goldbach n = head [(x, y) | x <- pr, y <- pr, x + y == n]
    where 
        pr = takeWhile (<n) primes

goldbachList :: Int -> Int -> [(Int, Int)]
goldbachList a b = map goldbach xs
    where
        ae = a + a `mod` 2
        be = b + b `mod` 2
        xs = [ae, ae+2..be]

goldbachList' :: Int -> Int -> Int -> [(Int, Int)]
goldbachList' a b c = filter (\(p1, p2) -> (p1 > c && p2 > c)) (goldbachList a b)

main = do
    print $ goldbachList 9 20
    print $ goldbachList' 4 2000 50
