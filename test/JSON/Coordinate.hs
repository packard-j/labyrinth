{-# LANGUAGE OverloadedStrings #-}
module JSON.Coordinate (coordinateTests) where
import Test.HUnit
import Maze.Coordinate
import Data.JSON.Coordinate
import Data.Aeson
import Data.ByteString.Lazy

decodeCoord :: ByteString -> Maybe JSONCoordinate
decodeCoord = decode

decodeValid :: Test
decodeValid = Just (JSONCoordinate $ Coordinate 0 0) ~=?
  decodeCoord "{\"row#\": 0, \"column#\": 0}"

decodeDecimal :: Test
decodeDecimal = Nothing ~=?
  decodeCoord "{\"row#\": 0.5, \"column#\": 1}"

decodeOutOfBounds :: Test
decodeOutOfBounds = Nothing ~=?
  decodeCoord "{\"row#\": 3, \"column#\": 7}"

coordinateTests :: Test
coordinateTests = TestList
  [ "valid coordinate (0,0)" ~: decodeValid,
    "invalid coordinate (0.5,1)" ~: decodeDecimal,
    "invalid coordinate (3, 7)" ~: decodeOutOfBounds]
