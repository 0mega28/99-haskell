data Tree a = Empty | Branch a (Tree a) (Tree a)
              deriving (Show, Eq)

leaf a = Branch a Empty Empty

hbalTree c 0 = [Empty]
hbalTree c 1 = [leaf c]
hbalTree c n = [Branch c l r |
                (hl, hr) <- [(n-2, n-1), (n-1, n-1), (n-1, n-2)],
                l <- hbalTree c hl, 
                r <- hbalTree c hr]

main = mapM print $ take 4 $ hbalTree 'x' 3