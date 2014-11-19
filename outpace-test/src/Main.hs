module Main where

import System.IO
import Data.List.Split (splitOn, chunksOf)

test :: [Char]
test = "    _  _     _  _  _  _  _\n\
       \  | _| _||_||_ |_   ||_||_|\n\
       \  ||_  _|  | _||_|  ||_| _|\n"



type AccountNumber = Int -- a big number with 9 digits
type Digit = Int -- a single digit
type SoupDigit = [Char] -- should be length 9 of: {_, |, space}
type SoupAccountNumber = [SoupDigit] -- account numbers have 9 digits


digitSoupToNumber :: SoupDigit -> Digit
digitSoupToNumber "     |  |" = 1
digitSoupToNumber " _  _||_ " = 2
digitSoupToNumber " _  _| _|" = 3
digitSoupToNumber "   |_|  |" = 4
digitSoupToNumber " _ |_  _|" = 5
digitSoupToNumber " _ |_ |_|" = 6
digitSoupToNumber " _   |  |" = 7
digitSoupToNumber " _ |_||_|" = 8
digitSoupToNumber " _|_| _|" = 9
-- 0
digitSoupToNumber _ = error "invalid digit"




tuplify3 :: [a] -> (a,a,a)
tuplify3 [x,y,z] = (x,y,z)
tuplify3 _ = error "list must have three elements"

mapTuple3 :: (a -> b) -> (a,a,a) -> (b,b,b)
mapTuple3 f (a,b,c) = (f a, f b, f c)

fromDigits :: [Int] -> Int
fromDigits = foldl addDigit 0
   where addDigit num d = 10 * num + d

parseAccountNumber :: String -> AccountNumber
parseAccountNumber soup = accountNumber
  where
    scannedLines = tuplify3 $ lines soup
    (topScans,middleScans,bottomScans) = mapTuple3 (chunksOf 3) scannedLines
    digitSoups = zipWith3 f topScans middleScans bottomScans
      where f t m b = t ++ m ++ b :: SoupDigit
    digits = map digitSoupToNumber digitSoups
    accountNumber = fromDigits digits

main :: IO ()
main = do
  handle <- openFile "sample.txt" ReadMode
  contents <- hGetContents handle
  let accountStrings = splitOn "\n\n" contents
  let accountNumbers = map parseAccountNumber accountStrings
  putStrLn (show accountNumbers)
  hClose handle
