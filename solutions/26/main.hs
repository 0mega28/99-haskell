combinations :: Int -> [a] -> [[a]]
combinations 0 _ = [[]]
combinations _ [] = []
combinations n (x:xs) =
    fmap (x :) (combinations (n-1) xs) ++ combinations n xs

main = print $ combinations 3 "abcdef"