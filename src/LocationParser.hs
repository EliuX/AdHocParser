module LocationParser(Identifier, Location(..), Point(..), parseSRID, parsePoint, parseLocation) where

import Data.Char(isSpace, isDigit)
import Data.List
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
parseSRID = Parser f
           where f ('S':'R':'I':'D':'=':xs)
                   | null ns         = Nothing
                   | otherwise       = case maybeValid ns of
                                         Just floatValue -> Just (floatValue, skipWhiteSpaces . skipSeparator . skipWhiteSpaces $ rest)
                                         _               -> Nothing
                     where (ns, rest) = span isDigit xs
                 f  _              = Nothing

-- Make a Parser for Point
parsePoint :: Parser Point
parsePoint  = Parser f
                     where f ('P':'O':'I':'N':'T':xs) = case unWrap '(' ')' xs of
                                                         Just x -> runParser (Point <$> parseFloat <*> parseFloat) x
                                                         _      -> Nothing
                           f  _                       = Nothing

-- Parses a location
parseLocation :: Parser Location
parseLocation = Location <$> parseSRID <*> parsePoint

parseFloat:: Parser Float
parseFloat = Parser f
  where
    f xs
      | null ns   = Nothing
      | otherwise = Just (read ns, skipWhiteSpaces rest)
      where (ns, rest) = span (not . isSpace) xs


-- Removes whitespaces at the edges
trim :: String -> String
trim = f . f
        where f = reverse . skipWhiteSpaces

skipWhiteSpaces :: String -> String
skipWhiteSpaces = dropWhile isSpace

skipSeparator :: String -> String
skipSeparator = dropWhile (==';')


-- Converts the parsed value a into b
second :: (c -> b) -> (a,c) -> (a,b)
second f (a,c) = (a, f c)

-- converts to requested element Maybe
maybeValid :: (Read a) => String -> Maybe a
maybeValid s = case reads s of
              [(x, "")] -> Just x
              _ -> Nothing

---- Removes the expected first and last item
unWrap :: Char -> Char -> String -> Maybe String
unWrap prefix suffix text = if head text == prefix && last text == suffix
                                then Just . tail . init $ text
                                else Nothing
