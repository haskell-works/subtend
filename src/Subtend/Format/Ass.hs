{-# LANGUAGE TypeFamilies #-}

module Subtend.Format.Ass where

import Conduit
import Control.Applicative
import Data.Attoparsec.Text
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

parseSectionName :: Parser String
parseSectionName = char '[' *> many (notChar '\n') <* char ']'
