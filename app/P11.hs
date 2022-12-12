{-# LANGUAGE LambdaCase #-}
module P11 where

import Control.Monad (forM_)
import Data.IORef    (IORef, newIORef, readIORef, writeIORef)

import Util

data Operation
  = OpAdd Integer
  | OpMul Integer
  | OpSquare
  deriving (Show)

data Monkey
  = Monkey
    { mItems       :: [Integer]
    , mOperation   :: Operation
    , mDivisor     :: Integer
    , mIfTrue      :: Int
    , mIfFalse     :: Int
    , mInspections :: Integer
    }
  deriving (Show)

parseMonkeys :: [String] -> IO [IORef Monkey]
parseMonkeys =
  chunksOf 7
  & mapM (\(
      _:
      starting:
      operation:
      test:
      ifTrue:
      ifFalse:
      _
    ) -> newIORef $ Monkey
      (filter (/= ',') starting % words % drop 2 % map read)
      (words operation % drop 4 % \case
        ["*", "old"] -> OpSquare
        ["*",  num ] -> read num % OpMul
        ["+",  num ] -> read num % OpAdd
        _            -> error "Invalid operation"
      )
      (words test    % (!! 3) % read)
      (words ifTrue  % (!! 5) % read)
      (words ifFalse % (!! 5) % read)
      0
  )

runOperation :: Operation -> Integer -> Integer
runOperation (OpAdd n) = (+ n)
runOperation (OpMul n) = (* n)
runOperation OpSquare  = \n -> n * n

runRound :: Integer -> [IORef Monkey] -> IO ()
runRound divisor monkeys = forM_ monkeys $ \mRef -> do
  monkey <- readIORef mRef
  let items = mItems monkey
  forM_ items $ \item -> do
    let newLevel = runOperation (mOperation monkey) item `div` divisor
    let throwIndex =
          if newLevel `mod` mDivisor monkey == 0
          then mIfTrue monkey
          else mIfFalse monkey
    updateAt throwIndex (updateRef (\m ->
      m {mItems = mItems m ++ [newLevel] })) monkeys
  monkey
    { mItems = []
    , mInspections = mInspections monkey + (length items % toEnum)
    } % writeIORef mRef
