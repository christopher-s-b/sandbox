module Main where

import System.IO
import Data.List.Split (splitOn, chunksOf)
import Data.Char (digitToInt)
--import Control.Monad (sequence)


type AccountNumber = [Digit] -- a big number with 9 digits
type Digit = Char -- a single digit
type SoupDigit = [Char] -- should be length 9 of: {_, |, space}
type SoupAccountNumber = [SoupDigit] -- account numbers have 9 digits

data ScanError = ILL | ERR deriving (Show)

digitSoupToNumber :: SoupDigit -> Either ScanError Digit
digitSoupToNumber "     |  |" = Right '1'
digitSoupToNumber " _  _||_ " = Right '2'
digitSoupToNumber " _  _| _|" = Right '3'
digitSoupToNumber "   |_|  |" = Right '4'
digitSoupToNumber " _ |_  _|" = Right '5'
digitSoupToNumber " _ |_ |_|" = Right '6'
digitSoupToNumber " _   |  |" = Right '7'
digitSoupToNumber " _ |_||_|" = Right '8'
digitSoupToNumber " _ |_| _|" = Right '9'
digitSoupToNumber " _ | ||_|" = Right '0'
digitSoupToNumber _           = Left ILL

tuplify3 :: [a] -> (a,a,a)
tuplify3 [x,y,z] = (x,y,z)
tuplify3 _ = error "list must have three elements"

mapTuple3 :: (a -> b) -> (a,a,a) -> (b,b,b)
mapTuple3 f (a,b,c) = (f a, f b, f c)

fromDigits :: [Int] -> Int
fromDigits = foldl addDigit 0
   where addDigit num d = 10 * num + d

parseAccountNumber :: String -> Either ScanError AccountNumber
parseAccountNumber soup = sequence digits
  where
    scannedLines = tuplify3 $ lines soup
    (topScans,middleScans,bottomScans) = mapTuple3 (chunksOf 3) scannedLines
    digitSoups = zipWith3 f topScans middleScans bottomScans
      where f t m b = t ++ m ++ b :: SoupDigit
    digits = map digitSoupToNumber digitSoups

dotProduct :: [Int] -> [Int] -> Int
dotProduct as bs = sum $ zipWith (*) as bs

isValid :: AccountNumber -> Bool
isValid a = dotProduct [1..9] (map digitToInt (reverse a)) `mod` 11 == 0

validate :: Either ScanError AccountNumber -> Either ScanError AccountNumber
validate e@(Left _) = e
validate e@(Right a) | isValid a = e
                     | otherwise = Left ERR

main :: IO ()
main = do
  handle <- openFile "sample.txt" ReadMode
  contents <- hGetContents handle
  let accountStrings = splitOn "\n\n" contents
  let accountNumbers = map parseAccountNumber accountStrings
  let xs = map validate accountNumbers
  putStrLn (show xs)
  hClose handle
