module P7 where

import Data.Functor (void)
import Data.List    (isPrefixOf)

import Stream
import Util

data File
  = File
    { fileName     :: String
    , fileSize     :: Integer
    , fileChildren :: [File]
    }
  deriving (Show)

parse :: Stream String File
parse = do
  dirName <- get & drop (length "$ cd ")
  void get -- ignore ls
  contents <-
    collect (head & (/= '$'))
    & filter (isPrefixOf "dir" & not)
    & map (words & \[size, name] -> File name (read size) [])
  File dirName (map fileSize contents % sum) contents % childrenScanner
  where
    childrenScanner :: File -> Stream String File
    childrenScanner file = do
      hasNext <- has
      if hasNext then do
        next <- get
        if next == "$ cd .." then pure file else do
          put next
          child <- parse
          let (File name size children) = file
          child : children % File name (fileSize child + size) % childrenScanner
      else
        pure file

dirs :: File -> [File]
dirs (File _ _ [])         = []
dirs f@(File _ _ children) = f : concatMap dirs children
