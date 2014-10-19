skips :: [a] -> [[a]]
skips xs = map (\n -> skip n xs) [1..(length xs)]

skip :: Integral n => n -> [a] -> [a]
skip n xs = map snd $ filter (\x -> (fst x) `mod` n == 0) (zip [1..] xs)
