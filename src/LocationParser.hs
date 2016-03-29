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
parseSRID = do
    void $ string "SRID="
    identifier <- many1 digit
    return (read identifier)

-- Make a Parser for Point
parsePoint :: Parser Point
parsePoint = do
      void $ string "POINT("
      p1 <- takeFloatNum
      atLeastOneWhiteSpace
      p2 <- takeFloatNum
      void $ char ')'
      return (Point p1 p2)

parseLocation :: Parser Location
parseLocation = Location <$> parseSRID <* expectSeparator <*> parsePoint


------- Complementary functions -------

-- Requires at least one whitespace
atLeastOneWhiteSpace :: Parser ()
atLeastOneWhiteSpace = void $ some $ oneOf " \n\t"

-- Expects a coma delimiting and some whitespaces
expectSeparator :: Parser ()
expectSeparator = do
      void spaces
      void $ char ';'
      void spaces

takeFloatNum :: Parser Float
takeFloatNum = do
            val <- many $ satisfy (\a -> isDigit a || a == '-' || a == '.')
            return (read val)