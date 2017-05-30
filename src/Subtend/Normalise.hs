module Subtend.Normalise where

class Normalise a where
  normalise :: a -> a
