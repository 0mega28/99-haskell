data Tree a = Empty | Branch a (Tree a) (Tree a)
              deriving (Show, Eq)

leaf a = Branch a Empty Empty

tree4 = Branch 1 (Branch 2 Empty (Branch 4 Empty Empty))
                 (Branch 2 Empty Empty)

{- An internal node of a binary tree has either one or two non-empty successors. 
    Write a predicate internals/2 to collect them in a list.  -}

internals Empty = []
internals (Branch _ Empty Empty) = []
internals (Branch a l r) = [a] ++ internals l ++ internals r

{- A node of a binary tree is at level N if the path from the root to the node has length N-1. 
    The root node is at level 1.
    Write a predicate atlevel/3 to collect all nodes at a given level in a list.  -}

atLevel :: Tree a -> Int -> [a]
atLevel Empty _ = []
atLevel (Branch a l r) 1 = [a]
atLevel (Branch _ l r) n = atLevel l (n-1) ++ atLevel r (n-1)

main = do 
    print $ internals tree4
    print $ atLevel tree4 2
