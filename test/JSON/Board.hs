{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module JSON.Board (boardTests) where
import Test.HUnit
import Examples.Board
import Data.JSON.Board
import Data.Aeson
import Data.Aeson.KeyMap
import Data.Aeson.QQ.Simple

parseBoard :: Value -> Result JSONBoard
parseBoard board = fromJSON $ addTreasures board (genTreasures 7 7)

addTreasures :: Value -> [[(String, String)]] -> Value
addTreasures (Object entries) treasures = object $ toList entries ++ [("treasures", toJSON treasures)]
addTreasures _ _ = error "not an object"

parseValid :: Test
parseValid = Success (JSONBoard board7x7) ~=? valid where
 valid = parseBoard [aesonQQ|{
  "connectors":
    [["│", "─", "┐", "└", "┌", "┘", "┬"],
     ["│", "─", "┐", "└", "┌", "┘", "┬"],
     ["│", "─", "┐", "└", "┌", "┘", "┬"],
     ["│", "─", "┐", "└", "┌", "┘", "┬"],
     ["│", "─", "┐", "└", "┌", "┘", "┬"],
     ["│", "─", "┐", "└", "┌", "┘", "┬"],
     ["│", "─", "┐", "└", "┌", "┘", "┬"]]
  }|]

parseDuplicate :: Test
parseDuplicate = Error "treasures are not mutually distinct" ~=? (board :: Result JSONBoard) where
 board = fromJSON $ addTreasures [aesonQQ|{
  "connectors":
    [["│", "─", "┐", "└", "┌", "┘", "┬"],
     ["│", "─", "┐", "└", "┌", "┘", "┬"],
     ["│", "─", "┐", "└", "┌", "┘", "┬"],
     ["│", "─", "┐", "└", "┌", "┘", "┬"],
     ["│", "─", "┐", "└", "┌", "┘", "┬"],
     ["│", "─", "┐", "└", "┌", "┘", "┬"],
     ["│", "─", "┐", "└", "┌", "┘", "┬"]]
  }|] (take 1 treasures ++ take 6 treasures)
  where treasures = genTreasures 7 7

parseInconsistentRow :: Test
parseInconsistentRow = Error "invalid size" ~=? parseBoard
  [aesonQQ| {"connectors":
  [["│", "─", "┐", "└", "┌", "┘", "┬"],
   ["│", "─", "┐", "└", "┌", "┘", "┬"],
   ["│", "─", "┐", "└", "┌", "┘", "┬"],
   ["│", "─", "┐", "└", "┌", "┘"],
   ["│", "─", "┐", "└", "┌", "┘", "┬"],
   ["│", "─", "┐", "└", "┌", "┘", "┬"],
   ["│", "─", "┐", "└", "┌", "┘", "┬"]]}|]

parseInvalidTile :: Test
parseInvalidTile = Error "invalid connector" ~=? parseBoard
  [aesonQQ| {"connectors":
  [["│", "─", "┐", "└", "┌", "┘", "┬"],
   ["│", "─", "┐", "└", "┌", "┘", "┬"],
   ["│", "─", "┐", "└", "┌", "┘", "┬"],
   ["│", "─", "┐", "└", "┌", "┘", "┬"],
   ["│", "─", "┐", "└", "┌", "┘", "┬"],
   ["│", "─", "┐", "└", "┌", ".", "┬"],
   ["│", "─", "┐", "└", "┌", "┘", "┬"]]}|]

parseTooSmall :: Test
parseTooSmall = TestList
  [ Error "invalid size" ~=? parseBoard [aesonQQ|{"connectors": []}|],
    Error "invalid size" ~=? parseBoard [aesonQQ|{"connectors": [[]]}|],
    Error "invalid size" ~=? parseBoard [aesonQQ|{"connectors": [["│"]]}|] ]

boardTests :: Test
boardTests = TestList
  [ "valid board" ~: parseValid,
    "inconsisent row length" ~: parseInconsistentRow,
    "invalid tile" ~: parseInvalidTile,
    "duplicate treasures" ~: parseDuplicate,
    "too small" ~: parseTooSmall ]
  
