
filter' p (x:xs)
    | p x        = x : filter' p xs
    | otherwise  = filter' p xs
filter' _ [] = []

odd' n = n `mod` 2 == 1
