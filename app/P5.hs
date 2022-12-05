module P5 where

import Data.List  (foldl', transpose)
import Data.Maybe (catMaybes)

import Util

type Crate   = Char
type Command = (Int, Int, Int)

parseLine :: String -> [Maybe Crate]
parseLine line = case line of
  ' ':' ':' ':rest -> Nothing : next rest
  '[': c :']':rest -> Just c  : next rest
  _                -> error "Not a crate"
  where
    next (' ':rest) = parseLine rest
    next _          = []

parseCrates :: [String] -> [[Crate]]
parseCrates = map parseLine & transpose & map catMaybes

parseCommands :: [String] -> [Command]
parseCommands =
  map (
    words
    & zip [0..]
    & filter (fst & odd) -- move A from B to C - all odd words are numbers
    & map    (snd & read)
    & \[count, from, to] -> (count, from - 1, to - 1) -- make zero-based
  )

runCommands :: ([Crate] -> [Crate]) -> [[Crate]] -> [Command] -> [[Crate]]
runCommands f = foldl' helper
  where
    helper crates (count, from, to) =
      crates
      % setAt from restStack
      % setAt to   newStack
      where
        (moving, restStack) =  (crates !! from) % splitAt count
        newStack = f moving ++ (crates !! to)
