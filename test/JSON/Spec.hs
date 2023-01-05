module JSON.Spec (jsonTests) where
import Test.HUnit
import JSON.Coordinate
import JSON.Board

jsonTests :: Test
jsonTests = TestList
  [ "coordinates" ~: coordinateTests,
    "board" ~: boardTests ]
