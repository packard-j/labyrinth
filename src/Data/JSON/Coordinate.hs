{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ApplicativeDo #-}
module Data.JSON.Coordinate (JSONCoordinate(..)) where
import Data.Aeson
import Data.Aeson.Types
import Data.Scientific
import Maze.Coordinate

newtype JSONCoordinate = JSONCoordinate Coordinate deriving (Show, Eq)

instance FromJSON JSONCoordinate where
  parseJSON value = JSONCoordinate <$> parseCoordinate value where
    parseCoordinate = withObject "Coordinate" $ \coord -> Coordinate
      <$> parseIndex (coord .: "column#")
      <*> parseIndex (coord .: "row#")
    parseIndex num = parseRange 0 6 $ parseInteger num

instance ToJSON JSONCoordinate where
  toJSON (JSONCoordinate (Coordinate x y)) = object ["row#" .= y, "column#" .= x]

-- | Parse an Integer from a Scientific, failing if the Scientific
-- | is not in the range of Int.
parseInteger :: Parser Scientific -> Parser Integer
parseInteger parser = do
  float <- parser
  case toBoundedInteger float :: Maybe Int of
    (Just i) -> pure $ fromIntegral i
    Nothing  -> fail "not an integer"

-- | Parse a value in the range from [lower, upper] inclusive.
parseRange :: (Ord a, Show a) => a -> a -> Parser a -> Parser a
parseRange lower upper parser = do
  x <- parser
  if x >= lower && x <= upper
    then pure x
    else fail $ "not in the range [" ++ show lower ++ "," ++ show upper ++ "]"
