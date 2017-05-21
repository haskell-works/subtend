module Subtend.Data.String where

import Data.Char

strip :: String -> String
strip = lstrip . rstrip

lstrip :: String -> String
lstrip = dropWhile isSpace

rstrip :: String -> String
rstrip = reverse . lstrip . reverse
