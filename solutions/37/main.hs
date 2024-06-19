import Data.List (group)

primeFactors = util 2
  where
    util :: Int -> Int -> [Int]
    util a b
      | b == 1 = []
      | b `mod` a == 0 = a : util a (b `div` a)
      | otherwise = util (a + 1) b

primeFactorMulti = map encode . group . primeFactors
  where
    encode xs = (head xs, length xs)

phi :: Int -> Int
phi = foldl encode 1 . primeFactorMulti
    where
        encode :: Int -> (Int, Int) -> Int
        encode a (f, m) = a * calc f m 
        calc :: Int -> Int -> Int
        calc f m = (f - 1) * f ^ (m - 1)

phi' n = product [(f - 1) * f ^ (m - 1) | (f, m) <- primeFactorMulti n]

main = do
    print $ phi 10
    print $ phi' 10
