encode :: Eq a => [a] -> [(Int, a)]
encode [] = []
encode [c] = [(1, c)]
encode xs@(x:ys) = (length eq, x) : encode neq
    where
        eq = takeWhile (== x) xs
        neq = dropWhile (== x) xs

data Encode = Multiple Int Char | Single Char
    deriving (Eq, Show)

encodeModified xs = map (\(f, c) -> if f == 1 then Single c else Multiple f c) (encode xs)
