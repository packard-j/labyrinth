import Test.HUnit
import BoardTest
import TileTest
import StateTest
import JSON.Spec

tests :: Test
tests = TestList
  [ "tiles" ~: tileTests,
    "board" ~: boardTests,
    "state" ~: stateTests,
    "json"  ~: jsonTests]

main :: IO ()
main = do
  _ <- runTestTT tests
  return ()

