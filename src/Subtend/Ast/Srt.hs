module Subtend.Ast.Srt where

import Data.Text

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

type Value = String
