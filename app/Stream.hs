{-# LANGUAGE TupleSections #-}
module Stream where

import Util

newtype Stream item result
  = Stream { runStream :: [item] -> ([item], result) }

instance Functor (Stream item) where
  fmap f (Stream as) = Stream $ \items0 ->
    let (items1, a) = as items0 in (items1, f a)

instance Applicative (Stream item) where
  pure a = Stream (, a)
  (Stream fs) <*> (Stream as) = Stream $ \items0 ->
    let
      (items1, f) = fs items0
      (items2, a) = as items1
    in
      (items2, f a)

instance Monad (Stream item) where
  (Stream as) >>= new = Stream $ \items0 ->
    let (items1, a) = as items0
    in new a % \(Stream bs) -> bs items1

has :: Stream a Bool
has = Stream $ \a -> (a, not $ null a)

get :: Stream a a
get = Stream $ \(h:t) -> (t, h)

put :: a -> Stream a ()
put a = Stream $ \as -> (a:as, ())

collect :: (a -> Bool) -> Stream a [a]
collect f = do
  hasNext <- has
  if hasNext then do
    next <- get
    if f next then collect f & (next:)
    else put next >> pure []
  else
    pure []
