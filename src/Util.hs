module Util(maybeDrop) where

maybeDrop :: Maybe Int -> [a] -> [a]
maybeDrop (Just n) = drop n
maybeDrop Nothing  = id