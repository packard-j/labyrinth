import Test.HUnit
import BoardTest
import TileTest

tests :: Test
tests = TestList
  [ "tiles" ~: tileTests,
    "board" ~: boardTests ]

main :: IO ()
main = do
  _ <- runTestTT tests
  return ()

