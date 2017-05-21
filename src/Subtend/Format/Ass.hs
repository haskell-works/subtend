{-# LANGUAGE TypeFamilies #-}

module Subtend.Format.Ass where

import Conduit
import Control.Applicative
import Data.Attoparsec.Text
import Data.Char
import Data.Conduit
import Data.Conduit.Combinators
import Data.Default
import Data.MonoTraversable
import Data.Sequences
import Prelude
import Subtend.Data.String

import qualified Data.Maybe as P
import qualified Prelude    as P

data Entry = Entry
  { key    :: String
  , values :: [String]
  } deriving (Eq, Show)

data Section = Section
  { name  :: String
  , entry :: [Entry]
  } deriving (Eq, Show)

newtype Document = Document
  { sections :: [Section]
  } deriving (Eq, Show)

parseLines :: Parser ()
parseLines = many (many (char ' ') *> endOfLine) >> pure ()

parseLines1 :: Parser ()
parseLines1 = many1 (many (char ' ') *> endOfLine) >> pure ()

parseSectionName :: Parser String
parseSectionName = char '[' *> many (satisfy (`P.notElem` ['\n', ']'])) <* char ']' <* parseLines

parseComment :: Parser ()
parseComment = (char ';' *> many (notChar '\n') <* parseLines) *> pure ()

parseIdentifier :: Parser String
parseIdentifier = many (letter <|> char ' ')

parseValue :: Parser String
parseValue = strip <$> many (satisfy (`P.notElem` "\n,"))

parseEntry :: Parser Entry
parseEntry = Entry <$> parseIdentifier <*> (char ':' *> sepBy parseValue (char ',') <* parseLines)

parseEmptyLine :: Default a => Parser a
parseEmptyLine = (skipSpace *> endOfLine) *> pure def

parseEntries :: Parser [Entry]
parseEntries = many parseEntry

parseSection :: Parser Section
parseSection = Section <$> parseSectionName <*> parseEntries

parseCommentOrSection :: Parser (Maybe Section)
parseCommentOrSection
  =   (Just           <$> parseSection)
  <|> (const Nothing  <$> parseComment)

parseDocument :: Parser Document
parseDocument = Document . P.catMaybes <$> many parseCommentOrSection
