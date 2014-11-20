module Main where

import System.IO
import Data.List.Split (splitOn, chunksOf)
import Data.Char (digitToInt)
import Data.List (intercalate)

type AccountNumber = [Digit] -- a big number with 9 digits
type Digit = Char -- a single digit
type SoupDigit = [Char] -- should be length 9 of: {_, |, space}
type SoupAccountNumber = [SoupDigit] -- account numbers have 9 digits

data ScanError = OK | ILL | ERR deriving (Show, Eq)

digitSoupToNumber :: SoupDigit -> (ScanError, Digit)
digitSoupToNumber "     |  |" = (OK, '1')
digitSoupToNumber " _  _||_ " = (OK, '2')
digitSoupToNumber " _  _| _|" = (OK, '3')
digitSoupToNumber "   |_|  |" = (OK, '4')
digitSoupToNumber " _ |_  _|" = (OK, '5')
digitSoupToNumber " _ |_ |_|" = (OK, '6')
digitSoupToNumber " _   |  |" = (OK, '7')
digitSoupToNumber " _ |_||_|" = (OK, '8')
digitSoupToNumber " _ |_| _|" = (OK, '9')
digitSoupToNumber " _ | ||_|" = (OK, '0')
digitSoupToNumber _           = (ILL, '?')

tuplify3 :: [a] -> (a,a,a)
tuplify3 [x,y,z] = (x,y,z)
tuplify3 _ = error "list must have three elements"

mapTuple3 :: (a -> b) -> (a,a,a) -> (b,b,b)
mapTuple3 f (a,b,c) = (f a, f b, f c)

fromDigits :: [Int] -> Int
fromDigits = foldl addDigit 0
   where addDigit num d = 10 * num + d

sequenceScans :: [(ScanError, Digit)] -> (ScanError, AccountNumber)
sequenceScans as | all (== OK) errors = (OK, digits)
                 | any (== ILL) errors = (ILL, digits)
                 | otherwise = error "impossible"
  where
    digits = (map snd as) :: AccountNumber
    errors = (map fst as) :: [ScanError]

parseAccountNumber :: String -> (ScanError, AccountNumber)
parseAccountNumber soup = sequenceScans digits
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

validate :: (ScanError, AccountNumber) -> (ScanError, AccountNumber)
validate p@(OK, a) | isValid a = p
                   | otherwise = (ERR, a)
validate p = p

formatResultLine :: (ScanError, AccountNumber) -> String
formatResultLine (e, a) = a ++ " " ++ render e
  where render OK = ""
        render ILL = show ILL
        render ERR = show ERR

main :: IO ()
main = do
  handle <- openFile "sample.txt" ReadMode
  contents <- hGetContents handle
  let accountStrings = splitOn "\n\n" contents
  let accountNumbers = map parseAccountNumber accountStrings
  let xs = map validate accountNumbers

  hOut <- openFile "results.txt" WriteMode
  hPutStrLn hOut $ intercalate "\n" (map formatResultLine xs)
  hClose handle
