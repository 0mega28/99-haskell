data Tree a = Empty | Branch a (Tree a) (Tree a)
              deriving (Show, Eq)

leaf a = Branch a Empty Empty

cbalTree 0 = [Empty]
cbalTree n = [ Branch defaultValue left right |
               i <- [q .. q+r],
               left <- cbalTree i,
               right <- cbalTree (n - 1 - i)
            ]
    where
        (q, r) = quotRem (n - 1) 2
        defaultValue = 'x'

main = mapM_ print $ cbalTree 4
