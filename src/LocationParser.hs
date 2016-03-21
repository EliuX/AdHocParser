module LocationParser(Identifier, Location, Point, parseSRID, parsePoint, takeLocation) where

import  AParser

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


data CantidateLog a = CantidateLog String
                      | Invalid
                      deriving Show

--instance Functor CantidateLog where
--   fmap f (CantidateLog logText) = case f . takeWhile (/=';') $ logText of
--                                        Nothing ->

-- Parses de identifier
parseSRID :: String -> Maybe Identifier
parseSRID ('S':'R':'I':'D':'=':x) = Just (read x)
parseSRID _                       = Nothing

-- Parses the Point
parsePoint :: String -> Maybe Point
parsePoint ('P':'O':'I':'N':'T':x) = takePoint content
                                     where content = map maybeFloat . splitIn ' ' <$> unWrap '[' ']' x
parsePoint _ = Nothing

takeLocation :: Maybe Identifier -> Maybe Point -> Maybe Location
takeLocation Nothing _ = Nothing
takeLocation _ Nothing = Nothing
takeLocation (Just x) (Just y) = Just (Location x y)

takePoint :: Maybe [Maybe Float] -> Maybe Point
takePoint (Just [Just x, Just y]) = Just (Point x y)
takePoint _ = Nothing

parseLocation :: String -> Maybe Location
parseLocation x = takeLocation <$> parseSRID <*> parsePoint $ x

-- Divide una candena en varios tokens a partir de un separador
splitIn:: Char -> String -> [String]
splitIn _ []  = []
splitIn c str = before:(splitIn c (drop 1 left))
       where (before, left) = break (==c) str

-- converts to float
maybeFloat :: String -> Maybe Float
maybeFloat s = case reads s of
              [(x, "")] -> Just x
              _ -> Nothing

-- Removes the expected first and last item
unWrap :: Char -> Char -> String -> Maybe String
unWrap prefix suffix text = if(head text == prefix && last text == suffix)
                                then Just . tail . init $ text
                                else Nothing