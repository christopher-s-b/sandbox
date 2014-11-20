module Main where

import System.IO
import BankOcr

main :: IO ()
main = do
  hIn <- openFile "sample.txt" ReadMode
  hOut <- openFile "results.txt" WriteMode
  contents <- hGetContents hIn
  hPutStrLn hOut $ parseBankOcrSoup contents
  hClose hIn
  hClose hOut
