import Data.List (group)

primeFactors = util 2
  where
    util :: Int -> Int -> [Int]
    util a b
      | b == 1 = []
      | b `mod` a == 0 = a : util a (b `div` a)
      | otherwise = util (a + 1) b

primeFactorMulti' = map encode . group . primeFactors
  where
    encode xs = (head xs, length xs)

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
  print $ primeFactorMulti' 1024
