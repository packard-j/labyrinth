module JSON.Spec (jsonTests, jsonProps) where
import Test.HUnit
import JSON.Coordinate
import JSON.Board

jsonTests :: Test
jsonTests = TestList
  [ "coordinates" ~: coordinateTests,
    "board" ~: boardTests ]

jsonProps :: IO Bool
jsonProps = and <$> sequence [ boardProps ]
