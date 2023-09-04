data NestedList a = Elem a | List [NestedList a]

flatten :: NestedList a -> [a]
flatten = helper []
    where
        helper :: [a] -> NestedList a -> [a]
        helper xs (Elem e) = xs ++ [e]
        helper xs (List l) = xs ++ foldl (\a x -> a ++ helper [] x) [] l
