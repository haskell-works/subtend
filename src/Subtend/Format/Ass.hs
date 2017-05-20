{-# LANGUAGE TypeFamilies #-}

module Subtend.Format.Ass where

import Data.Conduit
import Data.Conduit.Combinators
import Data.MonoTraversable
import Data.Sequences

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

xxx :: (Monad m, IsSequence seq, Element seq ~ Char) => Conduit seq m seq
xxx = linesUnbounded
