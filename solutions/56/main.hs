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

main = do
    print $ symmetric (Branch 'x' (Branch 'x' Empty Empty) Empty)
    print $ symmetric (Branch 'x' (Branch 'x' Empty Empty) (Branch 'x' Empty Empty))
