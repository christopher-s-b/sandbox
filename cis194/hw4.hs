-- exercise 1
fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs)
  | even x = (x - 2) * fun1 xs
  | otherwise = fun1 xs

fun1' :: [Integer] -> Integer
fun1' xs = foldl (*) 1 $ map (subtract 2) $ filter even xs

fun1'' :: [Integer] -> Integer
fun1'' = product . map (subtract 2) . filter even


fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n | even n    = n + fun2 (n `div` 2)
       | otherwise = fun2 (3 * n + 1)


-- -- exercise 2
-- data Tree a = Leaf
--             | Node Integer (Tree a) a (Tree a)
--             deriving (Show, Eq)

-- insertTree :: a -> Tree a -> Tree a
-- insertTree node | Leaf = Node 0 Leaf a Leaf
--                 | Node height l v r = case height of


--                     Node height l a (insertTree )

-- foldTree :: [a] -> Tree a
-- foldTree xs = foldr insertTree Leaf xs


-- exercise 3
xor :: [Bool] -> Bool
xor xs = foldr (\x acc -> if x then not acc else acc) False xs

map' :: (a -> b) -> [a] -> [b]
map' f xs = foldr (\x acc -> (f x):acc) [] xs
