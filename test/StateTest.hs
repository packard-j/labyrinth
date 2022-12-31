{-# LANGUAGE TupleSections #-}
module StateTest (stateTests) where
import Test.HUnit
import Examples.State
import Examples.Board
import State
import Coordinate
import Orientation

state3x3Move :: Test
state3x3Move = expected ~=? actual where
  actual = do
    state <- state3x3
    move state North 0 $ Coordinate 0 1 
  expected = (,False) <$> newStateWithSlide (Just (North, 0)) board3x3ShiftCol0U (tile '│') expectedPlayers
  expectedPlayers =
    [ PlayerPieces (Coordinate 1 1)
                   (Coordinate 1 1)
                   (Coordinate 2 1)
                   'b',
      PlayerPieces (Coordinate 1 1)
                   (Coordinate 1 1)
                   (Coordinate 0 1)
                   'a' ]

state3x3InvalidHome :: Test
state3x3InvalidHome = Nothing ~=? state where
  state = newState board3x3 (tile '│') players
  players = [ PlayerPieces (Coordinate 0 1)
                           (Coordinate 1 1)
                           (Coordinate 2 1)
                           'a' ]

state3x3Unreachable :: Test
state3x3Unreachable = Nothing ~=? invalidMove where
  invalidMove = do
    state <- state3x3
    move state North 0 $ Coordinate 0 0

state3x3Unmoved :: Test
state3x3Unmoved = Nothing ~=? actual where
  actual = do
    state <- state3x3
    move state East 2 $ Coordinate 0 2

stateTests :: Test
stateTests = TestList 
  [ "move player a to (0,1)" ~: state3x3Move,
    "cannot place a home on a movable tile" ~: state3x3InvalidHome,
    "cannot move to the same tile" ~: state3x3Unmoved,
    "cannot move player a to (0,0)" ~: state3x3Unreachable ]
