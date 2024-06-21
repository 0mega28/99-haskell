import Control.Monad (replicateM, join)
gray n = map join $ replicateM n ["0", "1"]

main = print $ gray 4
