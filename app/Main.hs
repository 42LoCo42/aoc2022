module Main where

import Data.List  (elemIndex, foldl1', intersect, sort)
import Data.Maybe (fromJust)

import P2
import P3
import P5
import P7
import P8
import Stream
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
      & map read
    )
    & \[a, b, c, d] -> f (a, b, c, d)
  )
  & filter (== True)
  & length

p4_1 :: IO Int
p4_1 = p4 (\(a, b, c, d) -> (a <= c && b >= d) || (c <= a && d >= b))

p4_2 :: IO Int
p4_2 = p4 (\(a, b, c, d) -> intersect [a..b] [c..d] % null % not)

--------------------------------------------------------------------------------

p5 :: ([Crate] -> [Crate]) -> IO [Crate]
p5 f = do
  [crateLines, _:commandLines] <-
    input 5
    & lines
    & splitWhen (take 2 & (== " 1")) -- split at stack index line

  let crates   = parseCrates   crateLines
  let commands = parseCommands commandLines

  runCommands f crates commands % map head % return

p5_1 :: IO [Crate]
p5_1 = p5 reverse

p5_2 :: IO [Crate]
p5_2 = p5 id

--------------------------------------------------------------------------------

p6 :: Int -> IO Int
p6 size =
  input 6
  & slidingWindow size
  & map (uniq & length)
  & elemIndex size
  & fromJust
  & (+ size) -- position is after marker

p6_1 :: IO Int
p6_1 = p6 4

p6_2 :: IO Int
p6_2 = p6 14

--------------------------------------------------------------------------------

p7 :: IO File
p7 =
  input 7
  & lines
  & runStream parse
  & snd

p7_1 :: IO Integer
p7_1 =
  p7
  & dirs
  & map fileSize
  & filter (<= 100000)
  & sum

p7_2 :: IO Integer
p7_2 = do
  root <- p7
  let needs = fileSize root - 40000000
  dirs root % map fileSize % filter (>= needs) % sort % head % pure

--------------------------------------------------------------------------------

p8 :: ([Int] -> [a]) -> (a -> a -> a) -> IO [a]
p8 rowFunc cellFunc =
  input 8
  & lines
  & map (map ((:[]) & (read :: String -> Int)))
  & runOnGrid rowFunc cellFunc
  & concat

p8_1 :: IO Int
p8_1 =
  p8 visibilityR (||)
  & filter id
  & length

p8_2 :: IO Int
p8_2 =
  p8 viewDists (*)
  & maximum
