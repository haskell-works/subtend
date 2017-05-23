module Subtend.Data.List where

find :: Eq a => (a -> Bool) -> [a] -> Maybe a
find p (a:as) | p a = Just a
find p (_:as) = find p as
find _ []     = Nothing
