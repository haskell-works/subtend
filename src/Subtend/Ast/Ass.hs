
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies      #-}

module Subtend.Ast.Ass where

import Data.Map
import Subtend.Data.ToMap

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
