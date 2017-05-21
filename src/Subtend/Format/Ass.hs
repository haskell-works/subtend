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

parseComment :: Parser String
parseComment = char ';' *> many (notChar '\n')

parseIdentifier :: Parser String
parseIdentifier = many letter

parseEntry :: Parser Entry
parseEntry = Entry <$> parseIdentifier <*> (char ':' *> many (notChar '\n'))
