{-# LANGUAGE FlexibleInstances #-}

module Subtend.Ast.Srt where

import Data.Text hiding (index)
import Subtend.Duration
import Subtend.Normalise

data Time = Time
  { hour    :: Int
  , minutes :: Int
  , seconds :: Int
  , millis  :: Int
  } deriving (Eq, Ord, Show)

data Frame = Frame
  { start :: Time
  , stop  :: Time
  } deriving (Eq, Ord, Show)

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

instance Normalise Document where
  normalise (Document entries) = Document (normalise entries)

instance Normalise [Entry] where
  normalise = go 1
    where go n (e:es) = e { index = n } : go (n + 1) es
          go _ _      = []
