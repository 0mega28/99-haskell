import Data.List (sortBy)
-- lsort :: [a] -> [a]
lsort = sortBy (\x y -> compare (length x) (length y))

main = print $ lsort ["abc","de","fgh","de","ijkl","mn","o"]