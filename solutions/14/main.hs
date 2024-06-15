dupli :: [a] -> [a]
dupli = foldr (\x a -> x:x:a) []
