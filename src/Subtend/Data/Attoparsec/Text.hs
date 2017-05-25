module Subtend.Data.Attoparsec.Text where

import Data.Attoparsec.Text
import Data.Char

decimalDigits :: Integral a => Int -> Parser a
decimalDigits = go 0
  where go a n = if n > 0
          then digit >>= \c -> go (a * 10 + fromIntegral (ord c - 48)) (n - 1)
          else pure a
