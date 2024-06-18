gcd' 0 b = b
gcd' a 0 = a
gcd' a b = abs $ gcd' b (a `mod` b)

main = do
    print $ gcd' 3 6
    print $ gcd' (-3) (-6)
    print $ gcd' (-3) 6