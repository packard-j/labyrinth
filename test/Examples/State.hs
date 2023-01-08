module Examples.State (state3x3) where
import Maze.State
import Maze.Coordinate
import Examples.Board

state3x3 :: StateResult (State Char ())
state3x3 = newState board3x3 (tile '└')
  [PlayerPieces (Coordinate 1 1)
                (Coordinate 1 1)
                (Coordinate 2 2)
                'a',
   PlayerPieces (Coordinate 1 1)
                (Coordinate 1 1)
                (Coordinate 2 1)
                'b']

