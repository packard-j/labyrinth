{-# LANGUAGE TupleSections #-}
module StateTest (stateTests) where
import Test.HUnit
import Examples.State
import Examples.Board
import State
import Rule
import Coordinate
import Orientation
import Control.Monad.Except

state3x3Move :: Test
state3x3Move = expected ~=? actual where
  actual = do
    state <- state3x3
    move state North 0 North $ Coordinate 0 1 
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

state3x3MoveRotate :: Test
state3x3MoveRotate = expected ~=? (state3x3 >>= \s -> move s North 0 South $ Coordinate 1 2) where
  expected = (,False) <$> newStateWithSlide (Just (North, 0)) board3x3ShiftCol0UAlt (tile '│') expectedPlayers
  expectedPlayers = 
    [ PlayerPieces (Coordinate 1 1)
                   (Coordinate 1 1)
                   (Coordinate 2 1)
                   'b',
      PlayerPieces (Coordinate 1 1)
                   (Coordinate 1 1)
                   (Coordinate 1 2)
                   'a' ]

state3x3InvalidHome :: Test
state3x3InvalidHome = throwError InvalidConfiguration ~=? state where
  state = newState board3x3 (tile '│') players
  players = [ PlayerPieces (Coordinate 0 1)
                           (Coordinate 1 1)
                           (Coordinate 2 1)
                           'a' ]

state3x3Unreachable :: Test
state3x3Unreachable = throwError (RuleBroken PathMustExist) ~=? invalidMove where
  invalidMove = do
    state <- state3x3
    move state North 0 North $ Coordinate 0 0

state3x3Unmoved :: Test
state3x3Unmoved = throwError (RuleBroken MustMoveToNewTile) ~=? actual where
  actual = do
    state <- state3x3
    move state East 2 North $ Coordinate 0 2

state3x3Kick :: Test
state3x3Kick = expected ~=? kick <$> state3x3 where
  expected = do
    state <- expectedState
    return (state, Just kickedPlayer)
  expectedState = newState board3x3 (tile '└') remainingPlayers
  remainingPlayers =
    [ PlayerPieces (Coordinate 1 1)
                   (Coordinate 1 1)
                   (Coordinate 2 1)
                   'b' ]
  kickedPlayer = 'a'

stateKickEmpty :: Test
stateKickEmpty = expected ~=? kick <$> emptyState where
  expected = do
    state <- emptyState
    return (state, Nothing)
  emptyState = newState board3x3 (tile '└') [] :: StateResult (State.State Char)

stateTests :: Test
stateTests = TestList 
  [ "move player a to (0,1)" ~: state3x3Move,
    "move player and rotate spare" ~: state3x3MoveRotate,
    "cannot place a home on a movable tile" ~: state3x3InvalidHome,
    "cannot move to the same tile" ~: state3x3Unmoved,
    "cannot move player a to (0,0)" ~: state3x3Unreachable,
    "kick player a" ~: state3x3Kick,
    "kick with no players remaining" ~: stateKickEmpty ]
