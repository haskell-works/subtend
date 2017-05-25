module Subtend.Duration where

newtype Duration = Duration
  { durationMillis :: Int
  } deriving (Eq, Show)

class ToDuration a where
  toDuration :: a -> Duration

class FromDuration a where
  fromDuration :: Duration -> a

viaDuration :: (FromDuration b, ToDuration a) => a -> b
viaDuration = fromDuration . toDuration
