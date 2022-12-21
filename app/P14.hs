{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TupleSections #-}
module P14 where

import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.List  (sort, sortOn)
import Data.Ord   (Down (Down))

import qualified Data.Set as S

import Util

type State = (Int, IORef (S.Set Pos))

parseRocks :: String -> IO State
parseRocks =
  lines
  & concatMap (
    words
    & filter ((== "->") & not)
    & map (
      splitWhen (== ',')
      & \[x, y] -> (read x, read y)
    )
    & \corners ->
    zipWith (\(x1, y1) (x2, y2) ->
      let gen a b f = [a, b] % sort % \[a', b'] -> map f [a'..b'] in
      if | x1 == x2  -> gen y1 y2 (x1,)
         | y1 == y2  -> gen x1 x2 (,y1)
         | otherwise -> error "Invalid corners!"
    ) corners (tail corners)
    % concat
  )
  & sortOn (snd & Down)
  & \rocks@((_, y):_) -> mapM newIORef (y, S.fromList rocks)

fallOne :: (Pos -> Bool) -> State -> IO Bool
fallOne contP (lowest, ref) =
  readIORef ref >>=
  \points -> do
    let result = helper points (500, 0)
    S.insert result points % writeIORef ref
    result % contP % pure
  where
    helper :: S.Set Pos -> Pos -> Pos
    helper points pos@(x, y) =
      [ (pos, y > lowest)
      , (helper points downPos,   downPos   `S.member` points % not)
      , (helper points downLePos, downLePos `S.member` points % not)
      , (helper points downRiPos, downRiPos `S.member` points % not)
      , (pos, True)
      ]
      % filter snd
      % head
      % fst
      where
        downPos   = (x    , y + 1)
        downLePos = (x - 1, y + 1)
        downRiPos = (x + 1, y + 1)
