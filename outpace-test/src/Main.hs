module Main where

import System.Environment
import System.IO
import BankOcr
import Options.Applicative

data BankOcrOpts = BankOcrOpts
  { infile :: String
  , outfile :: String }

sample :: Parser BankOcrOpts
sample = BankOcrOpts
  <$> strOption
      ( long "infile"
     <> metavar "FILE"
     <> help "input file containing raw Bank OCR scanner data" )
  <*> strOption
      ( long "outfile"
     <> metavar "FILE"
     <> help "output file for results" )

doit :: BankOcrOpts -> IO ()
doit (BankOcrOpts ifile ofile) = do
  hIn <- openFile ifile ReadMode
  hOut <- openFile ofile WriteMode
  contents <- hGetContents hIn
  hPutStrLn hOut $ parseBankOcrSoup contents
  hClose hIn
  hClose hOut

  
main :: IO ()
main = execParser opts >>= doit
  where
    opts = info (helper <*> sample)
      ( fullDesc
     <> progDesc "Parse and interpret Bank OCR scanner machine raw output"
     <> header "bankocrmain - a utility for parsing Bank OCR scanner raw output" )
