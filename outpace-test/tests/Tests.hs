module Main where

import BankOcr
import Test.Hspec


main :: IO ()
main = hspec $ do
  describe "Verify that BankOcr parser works" $ do
    it "can validate account numbers" $ do
      isValid "711111111" `shouldBe` True
      isValid "123456789" `shouldBe` True
      isValid "490867715" `shouldBe` True
      isValid "888888888" `shouldBe` False
      isValid "490067715" `shouldBe` False
      isValid "012345678" `shouldBe` False

    it "can parse a valid account" $ do      
      parseAccountNumber "\
\    _  _     _  _  _  _  _ \n\
\  | _| _||_||_ |_   ||_||_|\n\
\  ||_  _|  | _||_|  ||_| _|\n" `shouldBe` (OK, "123456789")

    it "can parse and recognize an invalid account" $ do
      parseAccountNumber "\
\    _  _     _  _  _  _  _ \n\
\  | _| _||_| _ |_   ||_||_|\n\
\  ||_  _|  | _||_|  ||_| _ \n" `shouldBe` (ILL, "1234?678?")

    it "can parse and recognize an account with bad checksum" $ do
      parseAccountNumber "\
\ _  _  _  _  _  _  _  _  _ \n\
\| || || || || || || || || |\n\
\|_||_||_||_||_||_||_||_||_|\n" `shouldBe` (ERR, "000000000")
