
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies      #-}

module Subtend.Ast.Ass where

import Data.Map
import Subtend.Data.ToMap
import Subtend.Duration

data Time = Time
  { hour    :: Int
  , minutes :: Int
  , seconds :: Int
  , centis  :: Int
  } deriving (Eq, Show)

instance ToDuration Time where
  toDuration (Time hour minutes seconds centis) = Duration (((((hour * 60) + minutes) * 60 + seconds) * 1000) + centis * 10)

instance FromDuration Time where
  fromDuration (Duration a) = Time
    { hour    = hours'
    , minutes = minutes'
    , seconds = seconds'
    , centis  = millis' `div` 10
    }
    where (b, millis' ) = a `divMod` 1000
          (c, seconds') = b `divMod` 60
          (d, minutes') = c `divMod` 60
          hours'        = d

newtype Values = Values [String] deriving (Eq, Show)

newtype Entries = Entries [Entry] deriving (Eq, Show)

data Entry = Entry
  { key    :: String
  , values :: [String]
  } deriving (Eq, Show)

data Section = Section
  { name    :: String
  , entries :: [Entry]
  } deriving (Eq, Show)

newtype Document = Document
  { sections :: [Section]
  } deriving (Eq, Show)

instance ToMap [Entry] where
  type MapKey [Entry]   = String
  type MapValue [Entry] = [Values]
  toMap (Entry key values:es) = unionWith (++) (singleton key [Values values]) (toMap es)
  toMap []                    = empty

instance ToMap [Section] where
  type MapKey [Section]   = String
  type MapValue [Section] = [Entry]
  toMap (Section name entry:es) = unionWith (++) (singleton name entry) (toMap es)
  toMap []                      = empty

-- instance ToMap [Value] where
--   type MapKey [Value]   = String
--   type MapValue [Value] = [Entry]
--   toMap (Section name entry:es) = unionWith (++) (singleton name entry) (toMap es)
--   toMap []                      = empty
