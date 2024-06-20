and' True True = True
and' _ _ = False

or' True _ = True
or' _ True = True
or' _ _ = False

nand' a b = not $ and' a b

xor' True False = True
xor' False True = True
xor' _ _ = False

equ' True True = True
equ' False False = True
equ' _ _ = False

table f = [show a ++ " " ++ show b ++ " " ++ show (f a b) 
            | a <- [True, False], b <- [True, False]]

main = do
  let result = table (\a b -> and' a $ or' a b)
  mapM_ putStrLn result
