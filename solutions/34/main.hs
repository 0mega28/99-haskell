coprime a b = gcd a b == 1
totient n = length $ filter (coprime n) [1..n]

main = print $ totient 10
