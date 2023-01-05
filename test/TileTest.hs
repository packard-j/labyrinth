module TileTest (tileTests) where
import Test.HUnit
import Maze.Tile
import Maze.Connector
import Maze.Orientation

barPlusConnected :: Test
barPlusConnected =
  tilesConnected (Tile Plus North) (Tile Bar East) East ~? 
  "─┼ should be connected" 

verticalLsConnected :: Test
verticalLsConnected =
  tilesConnected (Tile L North) (Tile  L South) North ~? 
  "┐\n└ should be connected"

verticalLsDisconnected :: Test
verticalLsDisconnected =
  not (tilesConnected (Tile L North) (Tile L South) South) ~? 
  "└\n┐ should not be connected"

lBarConnected :: Test
lBarConnected = 
  tilesConnected (Tile Bar East) (Tile L North) West ~?
  "└─ should be connected"

barLDisconnected :: Test
barLDisconnected =
  not (tilesConnected (Tile Bar East) (Tile L North) North) ~?
  "─└ should not be connected"

lTConnected :: Test
lTConnected =
  tilesConnected (Tile L South) (Tile T South) South ~?
  "┐\n┴ should be connected"

tileTests :: Test
tileTests = TestList
  [ "─┼ connected"      ~: barPlusConnected,
    "┐\n└ connected"    ~: verticalLsConnected,
    "└\n┐ disconnected" ~: verticalLsDisconnected,
    "└─ connected"      ~: lBarConnected,
    "─└ disconnected"   ~: barLDisconnected,
    "┐\n┴ connected"    ~: lTConnected ]
