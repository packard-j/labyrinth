{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ApplicativeDo #-}
module Data.JSON.Board (JSONBoard(..)) where
import Data.Aeson
import Data.Aeson.Types
import Data.Vector (toList)
import Maze.Board
import Maze.Tile
import Maze.Orientation
import Maze.Connector
import Maze.Coordinate

newtype JSONBoard = JSONBoard Board deriving (Show, Eq)

instance FromJSON JSONBoard where
  parseJSON value = JSONBoard <$> parseBoard value where
    parseBoard = withObject "Board" $ \board -> do
      connectors <- board .: "connectors"
      matrix <- parseMatrix parseConnector connectors
      tiles <- parseCoordinateMap rows cols matrix
      pure $ newBoard tiles rows cols
      -- TODO: parse "treasures"
      -- <*> parseMatrix (board .: "treasures") parseTreasure
    rows = 7
    cols = 7

type Matrix a = [[a]]

parseMatrix :: (Value -> Parser a) -> Value -> Parser (Matrix a)
parseMatrix parseElement = parseListOf "rows" (parseListOf "cols" parseElement)

parseListOf :: String -> (Value -> Parser a) -> Value -> Parser [a]
parseListOf name parseElement = withArray name $ \elements -> sequenceA (toList $ parseElement <$> elements)

parseCoordinateMap :: Integer -> Integer -> Matrix a -> Parser (Coordinate -> a)
parseCoordinateMap rows cols matrix
  | length matrix == fromIntegral rows &&
    all (\c -> length c == fromIntegral cols) matrix =
      pure access 
  | otherwise = fail "invalid size"
 where
  access (Coordinate col row) = matrix !! fromIntegral row !! fromIntegral col

parseConnector :: Value -> Parser Tile
parseConnector = withText "connector" tile where
  tile "│" = pure $ Tile Bar North
  tile "─" = pure $ Tile Bar East
  tile "└" = pure $ Tile L North
  tile "┌" = pure $ Tile L East
  tile "┐" = pure $ Tile L South
  tile "┘" = pure $ Tile L West
  tile "┬" = pure $ Tile T North
  tile "┤" = pure $ Tile T East
  tile "┴" = pure $ Tile T South
  tile "├" = pure $ Tile T West
  tile "┼" = pure $ Tile Plus North
  tile _   = fail "invalid connector" :: Parser Tile
