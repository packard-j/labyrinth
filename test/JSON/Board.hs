{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module JSON.Board (boardTests) where
import Test.HUnit
import Examples.Board
import Data.JSON.Board
import Data.Aeson
import Data.Aeson.QQ.Simple

parseBoard :: Value -> Result JSONBoard
parseBoard = fromJSON

parseValid :: Test
parseValid = Success (JSONBoard board7x7) ~=? parseBoard
  [aesonQQ| {"connectors":
  [["│", "─", "┐", "└", "┌", "┘", "┬"],
   ["│", "─", "┐", "└", "┌", "┘", "┬"],
   ["│", "─", "┐", "└", "┌", "┘", "┬"],
   ["│", "─", "┐", "└", "┌", "┘", "┬"],
   ["│", "─", "┐", "└", "┌", "┘", "┬"],
   ["│", "─", "┐", "└", "┌", "┘", "┬"],
   ["│", "─", "┐", "└", "┌", "┘", "┬"]]}|]

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
    "too small" ~: parseTooSmall ]
  
