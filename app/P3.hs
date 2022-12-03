module P3 where

import Data.Char (ord)
import Data.Ix   (inRange)

priority :: Char -> Int
priority c = ord c - offset c

offset :: Char -> Int
offset c
  | inRange ('a', 'z') c = ord 'a' - 1
  | inRange ('A', 'Z') c = ord 'A' - 27
  | otherwise = error "Invalid char"
