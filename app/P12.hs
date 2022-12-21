{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}
module P12 where

import Data.Char  (ord)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.List  (sortOn)
import Data.Maybe (catMaybes)
import Prelude    hiding (Left, Right)

import qualified Data.Vector as V

import Util

type Height = Int
type Dist = Int
type Cell = (Height, IORef Dist)
type Heightmap = V.Vector (V.Vector Cell)
data Dir = Up | Down | Left | Right deriving (Show)

parseHeightmap :: String -> IO Heightmap
parseHeightmap =
  lines
  & mapM (
    mapM (
      ord
      & subtract (ord 'a')
      & \h ->
      newIORef maxBound
      & (h,)
    )
    & fmap V.fromList
  )
  & fmap V.fromList

startNum :: Int
startNum = ord 'S' - ord 'a'

endNum :: Int
endNum = ord 'E' - ord 'a'

filterPos :: Heightmap -> Int -> [Pos]
filterPos hm val =
  hm
  % V.map (V.findIndices (fst & (== val)))
  % V.indexed
  % V.concatMap (\(y, xs) -> V.map (,y) xs)
  % V.toList

findPos :: Heightmap -> Int -> Pos
findPos hm = filterPos hm & head

move :: Dir -> Pos -> Pos
move Up    (x, y) = (x, y - 1)
move Down  (x, y) = (x, y + 1)
move Left  (x, y) = (x - 1, y)
move Right (x, y) = (x + 1, y)

at :: Heightmap -> Pos -> Maybe Cell
at hm (x, y) = hm V.!? y >>= (V.!? x)

canMove :: Int -> Int -> Bool
canMove from to
  | from == startNum = canMove 0 to
  | to   == endNum   = canMove from 25
  | otherwise        = to - from <= 1

invDir :: Dir -> Dir
invDir Up    = Down
invDir Down  = Up
invDir Left  = Right
invDir Right = Left

pathfind :: Bool -> Heightmap -> IO [Maybe [Dir]]
pathfind useAllA hm = mapM (helper 0) startPos
  where
    startPos = findPos hm startNum :
      if useAllA then filterPos hm 0 else []
    endPos   = findPos hm endNum
    helper :: Dist -> Pos -> IO (Maybe [Dir])
    helper dist pos
      | pos == endPos = pure $ Just []
      | otherwise     = case at hm pos of
      Nothing            -> pure Nothing
      Just (height, ref) ->
        readIORef ref >>= \cellDist ->
        if dist >= cellDist then pure Nothing else do
        writeIORef ref dist
        [Up, Down, Left, Right]
          % map (\dir ->
            let pos' = move dir pos in
            at hm pos' >>= \(other, _) ->
            if canMove height other % not then Nothing
            else sequence (dir, helper (dist + 1) pos') % Just
          )
          % catMaybes
          % sequence
          % fmap (
            map sequence
            & catMaybes
            & map (uncurry (:))
            & sortOn length
            & \case
              []    -> Nothing
              (h:_) -> Just h
          )
