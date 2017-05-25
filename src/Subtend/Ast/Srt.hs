module Subtend.Ast.Srt where

import Data.Text
import Subtend.Duration

data Time = Time
  { hour    :: Int
  , minutes :: Int
  , seconds :: Int
  , millis  :: Int
  } deriving (Eq, Show)

data Frame = Frame
  { start :: Time
  , stop  :: Time
  } deriving (Eq, Show)

data Entry = Entry
  { index    :: Int
  , frame    :: Frame
  , subtitle :: [Text]
  } deriving (Eq, Show)

instance ToDuration Time where
  toDuration (Time hour minutes seconds millis) = Duration (((((hour * 60) + minutes) * 60 + seconds) * 1000) + millis)

instance FromDuration Time where
  fromDuration (Duration a) = Time
    { hour    = hours'
    , minutes = minutes'
    , seconds = seconds'
    , millis  = millis'
    }
    where (b, millis' ) = a `divMod` 1000
          (c, seconds') = b `divMod` 60
          (d, minutes') = c `divMod` 60
          hours'        = d

newtype Document = Document [Entry] deriving (Eq, Show)

type Value = String
