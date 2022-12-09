module P9 where

import Util

data Pos2D
  = Pos2D
    { x :: Int
    , y :: Int
    }
  deriving (Eq, Ord, Show)

addPos2D :: Pos2D -> Pos2D -> Pos2D
addPos2D (Pos2D x1 y1) (Pos2D x2 y2) = Pos2D (x1 + x2) (y1 + y2)

isNear :: Pos2D -> Pos2D -> Bool
isNear (Pos2D x1 y1) (Pos2D x2 y2) =
  abs (x1 - x2) <= 1 && abs (y1 - y2) <= 1

nextStep :: Pos2D -> Pos2D -> Pos2D
nextStep h@(Pos2D hx hy) t@(Pos2D tx ty) =
  if isNear h t then t
  else Pos2D (signum (hx - tx)) (signum (hy - ty)) `addPos2D` t

parseMovement :: String -> [Pos2D]
parseMovement = words & \[dir, val] ->
  replicate (read val) $
    case dir of
      "U" -> Pos2D 0 1
      "D" -> Pos2D 0 (-1)
      "L" -> Pos2D (-1) 0
      "R" -> Pos2D 1 0
      _   -> error "Invalid direction"

runMovementR :: Pos2D -> [Pos2D] -> [Pos2D]
runMovementR move = foldUpdR (\point (action, firstRun) ->
  let point' =
        if firstRun
        then addPos2D point action
        else nextStep action point
  in ((point', False), point')) (move, True)
