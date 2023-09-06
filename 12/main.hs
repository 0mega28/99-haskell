encode :: Eq a => [a] -> [(Int, a)]
encode [] = []
encode [c] = [(1, c)]
encode xs@(x:ys) = (length eq, x) : encode neq
    where
        eq = takeWhile (== x) xs
        neq = dropWhile (== x) xs

encodeModified xs = map (\(f, c) -> if f == 1 then Single c else Multiple f c) (encode xs)


data Encode = Multiple Int Char | Single Char
    deriving (Eq, Show)

decodeModified :: [Encode] -> String
decodeModified [] = []
decodeModified ((Multiple f c):rest) = replicate f c ++ decodeModified rest
decodeModified ((Single c):rest) = c : decodeModified rest
