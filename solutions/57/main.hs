data Tree a = Empty | Branch a (Tree a) (Tree a)
              deriving (Show, Eq)

leaf a = Branch a Empty Empty

symmetric :: Tree a -> Bool
symmetric Empty = True
symmetric (Branch _ l r) = symmetric' l r
    where 
        symmetric' :: Tree a -> Tree b -> Bool
        symmetric' Empty Empty = True
        symmetric' _ Empty = False
        symmetric' Empty _ = False
        symmetric' (Branch _ l1 r1) (Branch _ l2 r2) = symmetric' l1 r2 && symmetric' r1 l2

construct :: Ord a => [a] -> Tree a
construct [] = Empty
construct xs = foldl insertNode Empty xs
    where
        insertNode Empty x = Branch x Empty Empty
        insertNode (Branch n l r) x
            | x <= n = Branch n (insertNode l x) r
            | otherwise = Branch n l (insertNode r x)

main = do
    let tree = construct [3, 2, 5, 7, 1]
    print tree
    print $ symmetric tree -- True

