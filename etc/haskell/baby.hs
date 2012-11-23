doubleMe x = x + x
doubleUs x y = doubleMe x + doubleMe y

doubleSmallNumber x = if x > 100 then x else x*2

boomBangs xs = [if x < 10 then "boom!" else "bang!" | x<-xs, odd x]

length' xs = sum [1 | _ <- xs]

factorial :: Integer -> Integer
factorial n = product [1..n]

lucky :: Int -> String
lucky 7 = "LUCKY!!!"
lucky x = "nope"

fac' :: Int -> Int
fac' 0 = 1
fac' n = n * fac' (n - 1)


head' :: [a] -> a
head' [] = error "bam"
head' (x:_) = x


max' :: (Ord a) => a -> a -> a
max' a b
 | a <= b = b
 | otherwise = a

bmiTell :: Double -> Double -> String
bmiTell weight height
 | bmi <= 18.5 = "emo"
 | bmi <= 25.0 = "ugly"
 | bmi <= 30.0 = "fat"
 | otherwise = "whale"
 where bmi = weight/height^2

bmiTell' :: Double -> Double -> String
bmiTell' weight height
 | bmi <= a = "emo"
 | bmi <= b = "ugly"
 | bmi <= c = "fat"
 | otherwise = "whale"
 where bmi = weight/height^2
       (a,b,c) = (18.5, 25.0, 30.0)

initials :: String -> String -> String
initials firstname lastname = [f] ++ ". " ++ [l] ++ "."
    where (f:_) = firstname
          (l:_) = lastname

initials' :: String -> String -> String
initials' (f:_) (l:_) = [f] ++ ". " ++ [l] ++ "."

calcBmis :: [(Double, Double)] -> [Double]
calcBmis xs = [bmi w h | (w,h) <- xs]
    where bmi w h = w/h^2

cyl :: Double -> Double -> Double
cyl r h =
    let sideArea = 2 * pi * r * h
        topArea = pi * r^2
    in sideArea + 2 * topArea


calcBmis' :: [(Double, Double)] -> [Double]
calcBmis' xs = [bmi | (w,h) <- xs, let bmi = w/h^2]

head'' :: [a] -> a
head'' [] = error "bam"
head'' (x:_) = x

head3 :: [a] -> a
head3 xs = case xs of [] -> error "bam"
                      (x:_) -> x

take' :: (Num i, Ord i) => i -> [a] -> [a]
take' n _ | n <= 0 = []
take' _ [] = []
take' n (x:xs) = x : take' (n-1) xs

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

repeat' :: a -> [a]
repeat' x = x:repeat' x

zip' :: [a] -> [b] -> [(a,b)]
zip' _ [] = []
zip' [] _ = []
zip' (x:xs) (y:ys) = (x,y):zip' xs ys

elem' :: (Eq a) => a -> [a] -> Bool
elem' a [] = False
elem' a (x:xs)
    | a == x = True
    | otherwise = a `elem'` xs

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
    let left = [a | a <- xs, a <= x]
        right = [a | a <- xs, a > x]
    in quicksort left ++ [x] ++ quicksort right

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ _ [] = []
zipWith' _ [] _ = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

flip' :: (a -> b -> c) -> (b -> a -> c)
flip' f = g
    where g x y= f y x

flip'' :: (a -> b -> c) -> b -> a -> c
flip'' f x y = f y x

map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) = f x : map f xs

comp :: (b -> c) -> (a -> b) -> a -> c
comp f g = \x -> f $ g x
