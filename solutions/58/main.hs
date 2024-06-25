data Tree a = Empty | Branch a (Tree a) (Tree a)
              deriving (Show, Eq)

leaf :: a -> Tree a
leaf a = Branch a Empty Empty

defaultValue = 'x'

cbalTree 0 = [Empty]
cbalTree n = [ Branch defaultValue left right |
               i <- [q .. q+r],
               left <- cbalTree i,
               right <- cbalTree (n - 1 - i)
            ]
    where
        (q, r) = quotRem (n - 1) 2


symCbalTree 0 = [Empty]
symCbalTree 1 = [leaf defaultValue]
symCbalTree n = [Branch defaultValue node $ invertNode node | node <- cbalTree (n `div` 2)]

invertNode :: Tree Char -> Tree Char
invertNode Empty = Empty
invertNode (Branch a l r) = Branch a (invertNode r) (invertNode l)

main = do
    mapM_ print $ symCbalTree 5
