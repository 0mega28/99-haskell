slice xs start end = take (end - start + 1) (drop (start - 1) xs)