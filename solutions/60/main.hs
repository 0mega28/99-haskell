data Tree a = Empty | Branch a (Tree a) (Tree a)
  deriving (Show, Eq)

leaf a = Branch a Empty Empty

maxNodes :: Int -> Int
maxNodes h = 2 ^ h - 1

fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

{-
    0   0     0
    1   1     1
    2   2     1
    3   4     2
    4   7     3
    5   12    5
    6   20    8
 -}
minNodes :: Int -> Int
minNodes h = fibs !! (h + 2) - 1

minHeight n = ceiling $ logBase 2 $ fromIntegral (n+1)
maxHeight n = length (takeWhile (<=(n + 1)) fibs) - 3

hbalTreeNodes x n = [t | h <- [minHeight n .. maxHeight n], t <- balTree h n]
  where
    -- Height balanced binary tree with n nodes
    balTree 0 n = [Empty]
    balTree 1 n = [leaf x]
    balTree h n = [Branch x l r | 
                    (hl, hr) <- [(h-2, h-1), (h-1, h-1), (h-1, h-2)],
                    let min_nl = max (minNodes hl) (n - 1 - maxNodes hr),
                    let max_nl = min (maxNodes hl) (n - 1 - minNodes hr),
                    nl <- [min_nl .. max_nl],
                    let nr = n - 1 - nl,
                    l <- balTree hl nl,
                    r <- balTree hr nr]

main = print $ length $ hbalTreeNodes 'x' 15
