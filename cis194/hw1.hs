main :: IO ()
main = print $ digits 1234

-- ccValidate :: Num a => [a] -> Bool
-- ccValidate digits = map f $ reverse digits where
--   f =

digits :: Integral a => a -> [a]
digits 0 = []
digits x = digits (x `div` 10) ++ [x `mod` 10]

-- reversed
digits' :: Integral a => a -> [a]
digits' 0 = []
digits' x = x `mod` 10 : digits' (x `div` 10)


doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther ds = [first, second*2] ++ doubleEveryOther rest where
  first = head ds
  second = head . tail $ ds
  rest = tail . tail $ ds

sumDigits :: [Integer] -> Integer
sumDigits xs = foldl (+) 0 xs

validate :: Integer -> Bool
validate x = (sumDigits $ doubleEveryOther $ reverse $ digits' x) == 0

-- excercise 5 - Tower of Hanoi
type Peg = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
