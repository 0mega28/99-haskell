import System.Random

removeAt n xs 
    | n < 0 = error "removeAt: negative argument"
    | n > length xs = error "removeAt: index out of range"
    | otherwise = take n xs ++ drop (n + 1) xs

rndPermu :: [a] -> IO [a]
rndPermu xs = diffSelect' (length xs) xs
    where
        diffSelect' 0 _ = return []
        diffSelect' _ [] = error "diffSelect: negative argument"
        diffSelect' n xs = do
            r <- randomRIO (0, length xs - 1)
            rest <- diffSelect' (n - 1) (removeAt r xs)
            return (xs !! r : rest)

main :: IO ()
main = rndPermu "abc" >>= putStrLn
