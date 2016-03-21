{-# OPTIONS_GHC -Wall #-}
module LocationParserSpec (main, spec) where

import Test.Hspec
import LocationParser

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "parseSRID" $ do
    it "parses the SRID of the location" $ do
      parseSRID "SRID=2380" `shouldBe` Just (2380)
      parseSRID "SRID2380" `shouldBe` Nothing
  describe "parsePoint" $ do
    it "parses the Point of the location" $ do
      parsePoint "POINT 2 3" `shouldBe` Nothing