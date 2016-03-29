{-# OPTIONS_GHC -Wall #-}
module LocationParserSpec (main, spec) where

import Test.Hspec
import Data.Either
import FunctionsAndTypesForParsing
import LocationParser

-- | @a `returnShouldSatisfy` p@ asserts that the action @a@ returns value that
--   satisfies predicate @p@.
returnShouldSatisfy :: (Show a, Eq a) => IO a -> (a -> Bool) -> Expectation
v `returnShouldSatisfy` p = v >>= (`shouldSatisfy` p)

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "parseSRID" $
   it "parses the SRID of the location" $ do
      regularParse parseSRID "SRID=2380" `shouldBe` Right 2380
--    regularParse parseSRID "SRID=2380" `shouldBe` Right (2380)
--    regularParse parseSRID "SRID2380" `returnShouldSatisfy` isLeft
--  describe "parsePoint" $ do
--    it "parses the Point of the location" $ do
--      regularParse parsePoint "POINT 2 3" `returnShouldSatisfy` isLeft
--      regularParse parsePoint "POINT(2 3)" `shouldBe` Right (Point 2 3)
--  describe "parseLocation" $ do
--    it "regular case of Location" $ do
--      regularParse parseLocation "SRID=2380;POINT(2 3)" `shouldBe` Right (Location { locSRID = 2380, locPoint = Point 2 3})
--    it "The identifier should not accept negative numbers" $ do
--      regularParse parseLocation "SRID=-2380;POINT(2 3)" `returnShouldSatisfy` isLeft
--    it "stric with boundaries" $ do
--      regularParse parseLocation "[SRID=2380;POINT(2 3)" `returnShouldSatisfy` isLeft
--      regularParse parseLocation "[SRID=2380;POINT(2 3)+" `returnShouldSatisfy` isLeft
--      regularParse parseLocation "SRID=2380;POINT(2 3)]" `returnShouldSatisfy` isLeft
--    it "accept negatives coordenate points" $ do
--      regularParse parseLocation "SRID=1234;POINT(-2 3.0)" `shouldBe` Right (Location { locSRID = 1234, locPoint = Point (-2) 3})
--    it "accept whitespaces on each segment" $ do
--      regularParse parseLocation "SRID=1234 ; POINT(-2 3.0)" `shouldBe` Right (Location { locSRID = 1234, locPoint = Point (-2) 3})
