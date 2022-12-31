import Test.HUnit
import BoardTest
import TileTest
import StateTest

tests :: Test
tests = TestList
  [ "tiles" ~: tileTests,
    "board" ~: boardTests,
    "state" ~: stateTests ]

main :: IO ()
main = do
  _ <- runTestTT tests
  return ()

