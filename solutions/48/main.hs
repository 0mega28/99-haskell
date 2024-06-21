import Control.Monad (replicateM)
-- table :: Int -> ([Bool] -> Bool) -> [String]
table n f = [toStr arg ++ " => " ++ show (f arg) | arg <- args n]
    where
        args n = replicateM n [True, False]
        toStr = unwords . map (\b -> show b ++ space b)
        space True = "  "
        space False = " "

and' = (&&)
or' = (||)
equ' = (==)

main = do
    let result = table 3 (\[a,b,c] -> a `and'` (b `or'` c) `equ'` a `and'` b `or'` a `and'` c)
    mapM_ putStrLn result