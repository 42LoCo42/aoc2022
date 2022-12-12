module Util where

import Data.Functor          ((<&>))
import Data.Functor.Identity (Identity (runIdentity))
import Data.IORef            (IORef, modifyIORef')
import Data.List             (group, sort)
import Text.Printf           (printf)

infixl 8 &
(&) :: Functor f => f a -> (a -> b) -> f b
(&) = (<&>)

infixl 0 %
(%) :: a -> (a -> b) -> b
(%) = flip ($)

input :: Int -> IO String
input day = readFile $ printf "inputs/%d" day

splitWhen :: (x -> Bool) -> [x] -> [[x]]
splitWhen f = foldr (\x (h:t) -> if f x then []:h:t else (x:h):t) [[]]

-- This is semi-equal to unfold, but I like it more
iterUpdate :: (a -> (b, a)) -> a -> [b]
iterUpdate f a = b : iterUpdate f a'
  where
    (b, a') = f a

chunksOf :: Int -> [x] -> [[x]]
chunksOf len = iterUpdate (splitAt len) & takeWhile (not . null)

setAt :: Int -> x -> [x] -> [x]
setAt n x = updateAt n (pure x % const) & runIdentity

updateAt :: Monad m => Int -> (x -> m x) -> [x] -> m [x]
updateAt 0 f    (h:t) = f h & (: t)
updateAt n f xs@(h:t)
  | n < 0     = pure xs
  | otherwise = updateAt (n - 1) f t & (h :)
updateAt _ _ [] = pure []

uniq :: Ord a => [a] -> [a]
uniq = sort & group & map head

slidingWindow :: Int -> [x] -> [[x]]
slidingWindow len xs@(_:t)
  | length window == len = window : slidingWindow len t
  | otherwise            = []
  where window = take len xs
slidingWindow _ [] = []

foldUpdR :: (a -> b -> (b, c)) -> b -> [a] -> [c]
foldUpdR f b0 = foldr (\a (b, cs) ->
  let (b1, c) = f a b in (b1, c:cs)) (b0, []) & snd

zipWith2D :: (a -> b -> c) -> [[a]] -> [[b]] -> [[c]]
zipWith2D = zipWith . zipWith

takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' _ []    = []
takeWhile' f (h:t) = h : if f h then takeWhile' f t else []

updateRef :: (x -> x) -> IORef x -> IO (IORef x)
updateRef f ref = modifyIORef' ref f >> pure ref
