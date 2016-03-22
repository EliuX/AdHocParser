module LocationParser(Identifier, Location(..), Point(..), parseSRID, parsePoint, parseLocation) where

import Data.Char(isSpace)

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


data Token a = Token a
               | Invalid
               deriving Show

class Box t where
  unbox ::  t a -> Maybe a

instance Box Token where
  unbox (Token a) = Just a
  unbox  _        = Nothing


--instance Functor CantidateLog where
--   fmap f (CantidateLog logText) = case f . takeWhile (/=';') $ logText of
--                                        Nothing ->

-- Parses de identifier
parseSRID :: String -> Maybe Identifier
parseSRID ('S':'R':'I':'D':'=':x) =   case maybeValid x of
                                          Just (idVal)     -> if idVal > 1
                                                              then Just idVal
                                                              else Nothing  
                                          _                 -> Nothing 
parseSRID _                           = Nothing

-- Parses the Point
parsePoint :: String -> Maybe Point
parsePoint ('P':'O':'I':'N':'T':x) = takePoint content
                                     where content = map maybeValid . words <$> unWrap '(' ')' x
parsePoint _ = Nothing

takeLocation :: Maybe Identifier -> Maybe Point -> Maybe Location
takeLocation Nothing _ = Nothing
takeLocation _ Nothing = Nothing
takeLocation (Just x) (Just y) = Just (Location x y)

takePoint :: Maybe [Maybe Float] -> Maybe Point
takePoint (Just [Just x, Just y]) = Just (Point x y)
takePoint _ = Nothing

-- Parsers the location by a given String, using parseSRID and parsePoint
parseLocation :: String -> Maybe Location
parseLocation text = takeLocation <$> parseSRID . head <*> parsePoint . last $ trim <$> splitIn ';' text

-- Divide una candena en varios tokens a partir de un separador
splitIn:: Char -> String -> [String]
splitIn _ []  = []
splitIn c str = before: splitIn c (drop 1 left)
       where (before, left) = break (==c) str

-- converts to float
maybeValid :: (Read a) => String -> Maybe a
maybeValid s = case reads s of
              [(x, "")] -> Just x
              _ -> Nothing

-- Removes the expected first and last item
unWrap :: Char -> Char -> String -> Maybe String
unWrap prefix suffix text = if head text == prefix && last text == suffix
                                then Just . tail . init $ text
                                else Nothing

-- Removes whitespaces at the edges
trim :: String -> String
trim = f . f
   where f = reverse . dropWhile isSpace                                