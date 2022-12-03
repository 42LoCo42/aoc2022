module P2 where

import Util

data RPS = Rock | Paper | Scissors deriving (Enum)
data Outcome = Draw | Win | Loss

readRPS :: String -> RPS
readRPS "A" = Rock
readRPS "B" = Paper
readRPS "C" = Scissors
readRPS "X" = Rock
readRPS "Y" = Paper
readRPS "Z" = Scissors
readRPS _   = error "Not a valid RockPaperScissors letter!"

readOutcome :: String -> Outcome
readOutcome "X" = Loss
readOutcome "Y" = Draw
readOutcome "Z" = Win
readOutcome _   = error "Not a valid Outcome letter!"

rpsVal :: RPS -> Int
rpsVal Rock     = 1
rpsVal Paper    = 2
rpsVal Scissors = 3

outcomeVal :: Outcome -> Int
outcomeVal Win  = 6
outcomeVal Draw = 3
outcomeVal Loss = 0

rpsOutcome :: RPS -> RPS -> Outcome
rpsOutcome opponent me =
  case (fromEnum me - fromEnum opponent) `mod` 3 of
    1 -> Win
    0 -> Draw
    _ -> Loss

rpsRound :: RPS -> RPS -> Int
rpsRound opponent me = rpsVal me + outcomeVal (rpsOutcome opponent me)

rpsSelect :: RPS -> Outcome -> RPS
rpsSelect opponent outcome = fromEnum opponent + val % flip mod 3 % toEnum
  where
    val = case outcome of
            Win  -> 1
            Draw -> 0
            Loss -> -1
