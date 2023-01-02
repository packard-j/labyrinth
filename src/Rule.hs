module Rule (Rule(..)) where

-- | Represents the rules that must be followed in the game.
data Rule = PathMustExist | CannotUndoPrevSlide | MustMoveToNewTile
  deriving (Show, Eq)
