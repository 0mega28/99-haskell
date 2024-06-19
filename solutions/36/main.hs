primeFactorMulti :: Int -> [(Int, Int)]
primeFactorMulti n = util 2 0 n
  where
    util :: Int -> Int -> Int -> [(Int, Int)]
    util _ 0 1 = []
    util i acc n
      | n `mod` i == 0 = util i (acc + 1) (n `div` i)
      | acc /= 0 = (i, acc) : util (i + 1) 0 n
      | otherwise = util (i + 1) 0 n

main = do 
    print $ primeFactorMulti 315
    print $ primeFactorMulti 48
    print $ primeFactorMulti 1024
