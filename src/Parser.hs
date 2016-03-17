module Parser(Identifier, Location, Point) where

-- 32 or 64 bits Integer specifying the Id
type Identifier = Int

-- Specify a location
data Location = Location { locSRID   :: Identifier
                          ,locName   :: String
                          ,locPoint  :: Point     }

-- 2D location point
data Point = Point { pointX :: Float
                    ,pointY :: Float }

-- Parses some String into a Valid location
--parseLocation :: String -> Location
--parseLocation

-- Parses some String into a Location
--parseLocationMessage :: String -> Maybe Location
--parseLocationMessage = parseLocationTokens . T.splitOn ";"

parseLocationTokens :: [String] -> Maybe Location
parseLocationTokens = Nothing

-- Parses de identifier
parseSRID :: String -> Maybe Identifier
parseSRID ("SRID=":value) = Just (read value)
parseSRID _ = Nothing

-- PArses de Point
-- parsePoint ::