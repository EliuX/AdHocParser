{-# OPTIONS_GHC -Wall #-}
module LocationParserSpec (main, spec) where

import Test.Hspec
import LocationParser
import AParser

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  let simpleParser = Parser (\x -> Just (x, ""))
      nullParser   = (Parser (\_ -> Nothing))::(Parser String)
  describe "Laws of Functor Parser" $ do
   it "1st functor law : fmap id = id" $ do
    runParser (fmap id (Parser (\x -> Just (0, x)))) "2" `shouldBe` runParser (id (Parser (\x -> Just (0, x)))) "2"
    runParser (fmap id nullParser) "2" `shouldBe` runParser (id nullParser) "2"
   it "2nd functor law : fmap (g . f) = fmap g . fmap f" $ do
    runParser (fmap ((++ " beautiful") . (++ " world")) simpleParser) "hello" `shouldBe` runParser ((++ " beautiful") <$> (++ " world") <$> (simpleParser)) "hello"
    runParser (fmap ((++ " beautiful") . (++ " world")) nullParser) "hello" `shouldBe` runParser ((++ " beautiful") <$> (++ " world") <$> nullParser) "hello"
  describe "Laws of Applicative Parser" $ do
   it "pure id <*> v = v -- Identity" $
    runParser (pure id <*> simpleParser) "sample" `shouldBe` runParser simpleParser "sample"
   it "pure f <*> pure x = pure (f x) -- Homomorphism" $ do
    runParser (pure ("Hola " ++) <*> pure " mundo") " resto" `shouldBe` runParser (pure (("Hola " ++) " mundo")) " resto"
   it "u <*> pure y = pure ($ y) <*> u -- Interchange" $ do
    runParser (pure ("Hola" ++) <*> pure " mundo") " resto" `shouldBe` runParser (pure ($ " mundo") <*> pure ("Hola" ++)) " resto"
   it "pure (.) <*> u <*> v <*> w = u <*> (v <*> w) -- Composition" $ do
    runParser (pure (.) <*> pure ("Hola " ++) <*> pure ("cruel " ++) <*> pure " mundo") " resto" `shouldBe` runParser (pure ("Hola " ++) <*> (pure ("cruel " ++) <*> pure " mundo")) " resto"
  describe "parseSRID" $
   it "parses the SRID of the location" $ do
    runParser parseSRID "SRID=2380" `shouldBe` Just (2380,"")
    runParser parseSRID "SRID2380"  `shouldBe` Nothing
  describe "parsePoint" $
    it "parses the Point of the location" $ do
      runParser parsePoint "POINT 2 3" `shouldBe` Nothing
      runParser parsePoint "POINT(2.0 3)" `shouldBe` Just (Point 2 3, "")
  describe "parseLocation" $ do
    it "regular case of Location" $
      runParser parseLocation "SRID=2380;POINT(2 3)" `shouldBe` Just (Location { locSRID = 2380, locPoint = Point 2 3}, "")
    it "The identifier should not accept negative numbers" $
      runParser parseLocation "SRID=-2380;POINT(2 3)" `shouldBe` Nothing
    it "accept valid values at the beginning" $ do
      runParser parseLocation "[SRID=2380;POINT(2 3)" `shouldBe` Nothing
      runParser parseLocation "[SRID=2380;POINT(2 3)+" `shouldBe` Nothing
      runParser parseLocation "SRID=2380;POINT(2 3)]" `shouldBe` Just (Location { locSRID = 2380, locPoint = Point 2 3}, "]")
    it "accept negatives coordenate points" $
      runParser parseLocation "SRID=1234;POINT(-2 3.0)" `shouldBe` Just (Location { locSRID = 1234, locPoint = Point (-2) 3}, "")
    it "do not accept whitespaces on each segment" $ do
      runParser parseLocation "SRID=1234 ; POINT(-2 3.0)" `shouldBe` Nothing
