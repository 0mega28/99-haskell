isPrime :: Int -> Bool
isPrime n = notElem 0 $ map (mod n) [2..n-1]

main = do
    print $ isPrime 7
    print $ isPrime 39