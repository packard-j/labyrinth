module Orientation (Orientation(..), rotateClockwiseBy) where

data Orientation = North
  | East
  | South
  | West
  deriving Eq

instance Ord Orientation where
  compare o1 o2 = compare (orientationToInt o1) (orientationToInt o2)

rotateClockwiseBy :: Orientation -> Orientation -> Orientation
rotateClockwiseBy orientation by = intToOrientation (sum $ map orientationToInt [orientation, by])

orientationToInt :: Orientation -> Integer
orientationToInt North   = 0
orientationToInt East  = 1
orientationToInt South = 2
orientationToInt West = 3

intToOrientation :: Integer -> Orientation
intToOrientation i = [North, East, South, West] !! (fromIntegral i `mod` 4)

