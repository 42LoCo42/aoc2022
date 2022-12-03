module Util where

import Data.Functor ((<&>))
import Text.Printf  (printf)

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

