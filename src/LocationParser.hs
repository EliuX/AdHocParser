module LocationParser(Identifier, Location(..), Point(..), parseSRID, parsePoint, parseFloat, parseLocation) where

import AParser

-- 32 or 64 bits Integer specifying the Id
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
parseSRID = char 'S' *>
            char 'R' *>
            char 'I' *>
            char 'D' *>
            char '=' *>
            posInt

-- Make a Parser for Point
parsePoint :: Parser Point
parsePoint  = Point <$> (
                      char 'P' *>
                      char 'O' *>
                      char 'I' *>
                      char 'N' *>
                      char 'T' *>
                      char '(' *>
                      parseFloat
                      ) <*> (
                      space *>
                      parseFloat
                      <* char ')'
                      )

-- Parses a location
parseLocation :: Parser Location
parseLocation = Location <$> parseSRID <*> (char ';' *> parsePoint)


parseFloat:: Parser Float
parseFloat = read <$> (char '-' `parseOrSkip` posInt `parseOrSkip` char '.' `parseOrSkip` posInt)


-- Expects an space
space :: Parser Char
space = char ' '