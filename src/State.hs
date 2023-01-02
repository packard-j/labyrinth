module State
  (State,
   StateResult, StateError(..),
   PlayerPieces(..),
   newState, newStateWithSlide, move, kick) where
import Rule
import Board
import Tile
import Coordinate
import Orientation
import Control.Monad.Except

-- | Represents the state of a game of labyrinth, including the
-- | game board, the current spare tile, the last slide action
-- | that occurred, and the list of players in the game. 
data State p = State
  { board :: Board,
    -- | The most recent slide action that has been performed by
    -- | a player, if one has occured yet. Otherwise, Nothing.
    lastSlide :: Maybe (Orientation, Integer),
    -- | The current spare tile that will be used on the next
    -- | slide action.
    spare :: Tile,
    -- | The list of players and their respective home, goal, and
    -- | position coordinates.
    -- | The player that is next to take a turn is at the head of
    -- | the list of players, and the remaining players follow in
    -- | turn-order.
    players :: [PlayerPieces p] } deriving (Show, Eq)

-- | Represents the player in a game of labyrinth and their
-- | respective home, goal, and current position on the board.
data PlayerPieces p = PlayerPieces
  { home     :: Coordinate,
    goal     :: Coordinate,
    position :: Coordinate,
    player   :: p } deriving (Show, Eq)

type StateResult = Except StateError
data StateError = NoPlayers | InvalidConfiguration | RuleBroken Rule | InvalidMove BoardError
  deriving (Show, Eq)

-- | Construct the state for a new game of labyrinth to be played on
-- | the given board, with the specified spare tile and players.
-- | The player pieces must be located on the board, and each player's
-- | home and goal must be located on fixed tiles.
newState :: Board -> Tile -> [PlayerPieces p] -> StateResult (State p)
newState = newStateWithSlide Nothing

-- | Construct a state with the specified previous slide action.
newStateWithSlide :: Maybe (Orientation, Integer) -> Board -> Tile -> [PlayerPieces p] -> StateResult (State p)
newStateWithSlide prevSlide gameBoard spareTile playerPieces
  | not $ all (isOnBoard gameBoard) coords = throwError InvalidConfiguration 
  | not $ all (isOnFixedTile gameBoard) homesAndGoals = throwError InvalidConfiguration
  | otherwise = return $ State gameBoard prevSlide spareTile playerPieces
    where coords = position <$> playerPieces
          homesAndGoals = concatMap (\p -> [home p, goal p]) playerPieces

-- | Perform a slide and move action as the current player in the turn-order,
-- | producing the new state and whether the action resulted in the player
-- | landing on its goal tile.
-- | The slide is specified by the orientation to slide towards and the index
-- | of the axis. The destination to move the current player to is specified
-- | by the coordinate.
-- | Nothing is returned if:
-- |   * no players exist in the state
-- |   * the slide cannot be performed
-- |   * the player cannot move to the specified destination after the slide
-- |   * the specified destination is the same as the player's position after
-- |     the slide.
move :: State p -> Orientation -> Integer -> Coordinate -> StateResult (State p, Bool)
move state dir axis to 
  | null $ players state = throwError NoPlayers
  | lastSlide state == Just (rotateClockwiseBy dir South, axis) = throwError $ RuleBroken CannotUndoPrevSlide
  | otherwise = do
    (shiftedBoard, nextSpare) <- withExcept InvalidMove $ slide (board state) (spare state) dir axis
    updatedPlayers <- updatePlayers shiftedBoard (players state) dir axis to
    return (State shiftedBoard slideAction nextSpare (rotate updatedPlayers),
                  reachedGoal $ head updatedPlayers) where
      reachedGoal p = position p == goal p
      slideAction = Just (dir, axis)

-- | Remove the current player from the game state.
-- | Produces the game state without that player, and the player (if the 
-- | state had any players)
kick :: State p -> (State p, Maybe p)
kick state
  | null $ players state = (state, Nothing)
  | otherwise = (state { players = tail $ players state }, Just $ player $ head $ players state)

-- | Update the player positions in response to a slide and move action.
-- | All players on the affected row or column will be shifted, then the current
-- | player will be moved to the destination, if possible.
-- | Nothing is returned if:
-- |   * the destination tile cannot be reached from the player's position
-- |     after sliding.
-- |   * the player is already at the destination after the slide.
updatePlayers :: Board -> [PlayerPieces p] -> Orientation -> Integer -> Coordinate -> StateResult [PlayerPieces p]
updatePlayers _ [] _ _ _ = return []
updatePlayers brd (p0:rest) dir axis to = do
  moved <- movePlayer brd playerAfterShift to
  if position moved == position playerAfterShift
     then throwError $ RuleBroken MustMoveToNewTile
     else return $ moved:tail shifted
  where
    shifted = shiftPlayer brd dir axis <$> p0:rest
    playerAfterShift = head shifted

-- | Shift a player's position along with a slide action on a board, if it was affected.
shiftPlayer :: Board -> Orientation -> Integer -> PlayerPieces p -> PlayerPieces p
shiftPlayer b dir axis p = p 
  { position = shiftAlong b dir axis (position p) }

-- | Move a player to the specified coordinate on the board, if possible.
movePlayer :: Board -> PlayerPieces p -> Coordinate -> StateResult (PlayerPieces p)
movePlayer gameBoard pieces to = do
  exists <- withExcept InvalidMove $ pathExists gameBoard (position pieces) to
  if exists then return $ pieces { position = to }
            else throwError $ RuleBroken PathMustExist

-- | Rotate a list by moving the item at the head of the list to the back.
rotate :: [a] -> [a]
rotate (x:xs) = xs ++ [x]
rotate empty  = empty
