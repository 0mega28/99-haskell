import Data.List (sortBy, insertBy)
import Data.Ord (comparing)

data HTree a = Leaf a | Branch (HTree a) (HTree a)
  deriving (Show)

-- huffman :: (Ord a, Ord b, Num b) => [(a, b)] -> [(a, [Char])]
huffman freq = sortBy (comparing fst) $ serialize $ htree $ sortBy (comparing fst) [(b, Leaf a) | (a, b) <- freq]
    where
        htree [(_, t)] = t
        htree ((w1, t1):(w2, t2):ws) = htree $ insertBy (comparing fst) (w1 + w2, Branch t1 t2) ws
        serialize (Branch l r) = 
            [(x, '0':code) | (x, code) <- serialize l] ++ [(x, '1':code) | (x, code) <- serialize r]
        serialize (Leaf x) = [(x, "")]
        

main = print $ huffman [('a', 45), ('b', 13), ('c', 12), ('d', 16), ('e', 9), ('f', 5)]
