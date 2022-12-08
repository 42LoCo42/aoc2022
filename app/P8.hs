module P8 where

import Data.List (tails, transpose)

import Util

visibilityR :: [Int] -> [Bool]
visibilityR = foldUpdR (\i highest ->
  if i > highest then (i, True) else (highest, False)) (-1)

viewDists :: [Int] -> [Int]
viewDists =
  tails
  & filter (not . null)
  & map (\(h:t) ->
    takeWhile' (< h) t
    % length
  )

runOnGrid :: ([Int] -> [a]) -> (a -> a -> a) -> [[Int]] -> [[a]]
runOnGrid rowFunc cellFunc = withTransposed
  where
    combine = zipWith2D cellFunc
    forOne  = map rowFunc
    withReversed   g = combine (forOne g)       (map reverse g % forOne       % map reverse)
    withTransposed g = combine (withReversed g) (transpose   g % withReversed % transpose  )
