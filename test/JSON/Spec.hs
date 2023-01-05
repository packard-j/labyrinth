module JSON.Spec (jsonTests) where
import Test.HUnit
import JSON.Coordinate

jsonTests :: Test
jsonTests = TestList
  [ "coordinates" ~: coordinateTests ]
