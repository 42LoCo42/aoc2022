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

--------------------------------------------------------------------------------

p4 :: ((Int, Int, Int, Int) -> Bool) -> IO Int
p4 f =
  input 4
  & lines
  & map (
    splitWhen (== ',')
    & concatMap (
      splitWhen (== '-')
      & map (read :: String -> Int)
    )
    & \[a, b, c, d] -> f (a, b, c, d)
  )
  & filter (== True)
  & length

p4_1 :: IO Int
p4_1 = p4 (\(a, b, c, d) -> (a <= c && b >= d) || (c <= a && d >= b))

p4_2 :: IO Int
p4_2 = p4 (\(a, b, c, d) -> intersect [a..b] [c..d] % null % not)
