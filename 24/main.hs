import System.Random
import GHC.Base (IO(IO))

removeAt n xs 
    | n < 0 = error "removeAt: negative argument"
    | n > length xs = error "removeAt: index out of range"
    | otherwise = take n xs ++ drop (n + 1) xs

diffSelect :: Int -> Int -> IO [Int]
diffSelect n m = diffSelect' n [1..m]
    where
        diffSelect' 0 _ = return []
        diffSelect' _ [] = error "diffSelect: negative argument"
        diffSelect' n xs = do
            r <- randomRIO (0, length xs - 1)
            rest <- diffSelect' (n - 1) (removeAt r xs)
            return (xs !! r : rest)

main = do
    xs <- diffSelect 5 10
    print xs
