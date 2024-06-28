data Tree a = Empty | Branch a (Tree a) (Tree a)
              deriving (Show, Eq)

leaf a = Branch a Empty Empty

x = 'x'

completeBinaryTree n = generate_tree 1                                                                                                                       
  where generate_tree x                                                                                                                                        
          | x > n     = Empty                                                                                                                                  
          | otherwise = Branch 'x' (generate_tree (2*x)  )                                                                                                     
                                   (generate_tree (2*x+1)) 

isCompleteBinaryTree Empty = True
isCompleteBinaryTree (Branch _ _ Empty) = True
isCompleteBinaryTree (Branch _ Empty _) = False
isCompleteBinaryTree (Branch _ l r) = isCompleteBinaryTree l && isCompleteBinaryTree r

main = do
    let tree = completeBinaryTree 4
    let isComplete = isCompleteBinaryTree tree
    print tree
    print isComplete
    