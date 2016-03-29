module LocationParser(Identifier, Location(..), Point(..), parseSRID, parsePoint, parseLocation) where


import Control.Applicative (many, some)
import Control.Monad (void)
import Data.Char
import FunctionsAndTypesForParsing
import Text.Parsec.String (Parser)
import Text.Parsec.String.Char
import Text.Parsec.String.Combinator (many1)

-- 32 bits Integer specifying the Id
type Identifier = Int

-- Specify a location
data Location = Location { locSRID  :: Identifier
                          ,locPoint :: Point   }
                           deriving (Show, Eq)

-- 2D location point
data Point = Point { pointX :: Float
                    ,pointY :: Float }
                     deriving (Show, Eq)

-- make a Parser for an Identifier
parseSRID :: Parser Identifier
parseSRID = read <$> (string "SRID=" *> many1 digit)

-- Make a Parser for Point
parsePoint :: Parser Point
parsePoint = Point <$> (string "POINT(" *> takeFloatNum) <*> (char ' ' *> takeFloatNum <* char ')')

parseLocation :: Parser Location
parseLocation = Location <$> parseSRID <* expectSeparator <*> parsePoint


------- Complementary functions -------

-- Expects a coma delimiting and some whitespaces
expectSeparator :: Parser ()
expectSeparator =  spaces <* char ';' <* spaces

takeFloatNum :: Parser Float
takeFloatNum = read <$> (many $ satisfy (\a -> isDigit a || a == '-' || a == '.'))
