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
      parsePoint "POINT(2 3)" `shouldBe` Just (Point 2 3) 
  describe "parseLocation" $ do 
    it "regular case of Location" $ do
     parseLocation "SRID=2380;POINT(2 3)" `shouldBe` Just (Location { locSRID = 2380, locPoint = Point 2 3})     
    it "The identifier should not accept negative numbers" $ do
     parseLocation "SRID=-2380;POINT(2 3)" `shouldBe` Nothing
    it "stric with boundaries" $ do 
     parseLocation "[SRID=2380;POINT(2 3)" `shouldBe` Nothing
     parseLocation "[SRID=2380;POINT(2 3)+" `shouldBe` Nothing
     parseLocation "SRID=2380;POINT(2 3)]" `shouldBe` Nothing
    it "accept negatives coordenate points" $ do
     parseLocation "SRID=1234;POINT(-2 3.0)" `shouldBe` Just (Location { locSRID = 1234, locPoint = Point (-2) 3}) 
    it "accept whitespaces on each segment" $ do
     parseLocation "SRID=1234 ; POINT(-2 3.0)" `shouldBe` Just (Location { locSRID = 1234, locPoint = Point (-2) 3}) 