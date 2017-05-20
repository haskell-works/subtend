module Subtend.Format.Ass where

data Entry = Entry
  { key   :: String
  , value :: String
  } deriving (Eq, Show)

data Section = Section
  { name  :: String
  , entry :: [Entry]
  } deriving (Eq, Show)

newtype Document = Document
  { sections :: [Section]
  } deriving (Eq, Show)
