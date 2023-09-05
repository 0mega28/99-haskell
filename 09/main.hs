pack :: (Eq a) => [a] -> [[a]]
pack xs = helper xs [] []
    where
        helper :: (Eq a) => [a] -> [a] -> [[a]] -> [[a]]
        helper [] la ga = ga ++ [la]
        helper (x:ys) [] ga = helper ys [x] ga
        helper xs@(x:ys) la@(l:_) ga
            | x == l = helper ys (la ++ [x]) ga
            | otherwise = helper xs [] (ga ++ [la])
