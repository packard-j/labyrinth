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

props :: IO Bool
props = and <$> sequence [ jsonProps ]

main :: IO ()
main = do
  _ <- runTestTT tests
  _ <- props
  return ()

