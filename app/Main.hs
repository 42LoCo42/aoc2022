module Main where

import Data.List (foldl1', intersect, sort)

import P2
import P3
import Util

main :: IO ()
main = error "call puzzle functions directly from ghci"

--------------------------------------------------------------------------------

p1 :: IO [Int]
p1 =
  input 1
  & lines
  & splitWhen null
  & map (map read & sum)

p1_1 :: IO Int
p1_1 = p1 & maximum

p1_2 :: IO Int
p1_2 = p1 & sort & reverse & take 3 & sum

--------------------------------------------------------------------------------

p2 :: ([String] -> Int) -> IO Int
p2 f =
  input 2
  & lines
  & map (words & f)
  & sum

p2_1 :: IO Int
p2_1 = p2 (map readRPS & \[opponent, me] -> rpsRound opponent me)

p2_2 :: IO Int
p2_2 = p2 (\[opponent, me] -> helper (readRPS opponent) (readOutcome me))
  where
    helper :: RPS -> Outcome -> Int
    helper opponent me = rpsSelect opponent me % rpsRound opponent

--------------------------------------------------------------------------------

p3_1 :: IO Int
p3_1 =
  input 3
  & lines
  & map (\l ->
    splitAt (length l `div` 2) l
    % uncurry intersect
    % head
    % priority
  )
  & sum

p3_2 :: IO Int
p3_2 =
  input 3
  & lines
  & chunksOf 3
  & map (foldl1' intersect & head & priority)
  & sum
