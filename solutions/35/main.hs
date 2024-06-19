primeFactors = util 2
    where
    util :: Int -> Int -> [Int]
    util a b
        | b == 1 = []
        | b `mod` a == 0 = a : util a (b `div` a)
        | otherwise = util (a + 1) b

main = print $ primeFactors 48
