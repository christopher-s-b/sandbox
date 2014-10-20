-- exercise 1
skips :: [a] -> [[a]]
skips xs = map (\n -> skip n xs) [1..(length xs)]

skip :: Integral n => n -> [a] -> [a]
skip n xs = map snd $ filter (\x -> (fst x) `mod` n == 0) (zip [1..] xs)


--exercise 2
isLocalMaximum :: Integral a => (a,a,a) -> Bool
isLocalMaximum (a,b,c) = b > a && b > c

sliding3 :: [a] -> [(a,a,a)]
sliding3 xs@(a:b:c:_) = (a,b,c) : sliding3 (tail xs)
sliding3 _ = []

localMaxima :: Integral a => [a] -> [a]
localMaxima xs = map proj2 $ filter isLocalMaximum (sliding3 xs)
  where proj2 (_,b,_) = b

-- *Main> filter isLocalMaximum (sliding3 [1,5,2,6,3])
-- [(1,5,2),(2,6,3)]


-- another way
filterBy :: (b -> Bool) -> ([a] -> [b]) -> [a] -> [a]
filterBy p f as = as'
  where indexedAs = zipWith (,) [0..] as
        indexedBs = zipWith (,) [0..] (f as)
        indexedBs' = filter p indexedBs
        indexes = map fst indexedBs
        as' = map (\i -> snd (indexedAs !! i)) indexes

localMaxima' :: Integral a => [a] -> [a]
localMaxima' xs = filterBy isLocalMaximum sliding3 xs
