{-# LANGUAGE RankNTypes #-}

module Subtend.Format.Srt where

import Conduit
import Control.Applicative
import Data.Attoparsec.Text
import Data.Conduit
import Data.Text
import System.IO

import qualified Data.Attoparsec.Text as A
import qualified Data.ByteString      as BS
import qualified Data.Conduit         as C
import qualified Data.Text            as T

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

parseIndex :: Parser Int
parseIndex = decimal <* endOfLine

parseFrame :: Parser Frame
parseFrame = Frame <$> parseTime <* string (T.pack " --> ") <*> parseTime <* skipSpace

parseTime :: Parser Time
parseTime = Time <$> decimal <* char ':' <*> decimal <* char ':' <*> decimal <* char ',' <*> decimal

parseSubtitleLine :: Parser Text
parseSubtitleLine = takeWhile1 (not . isEndOfLine) <* endOfLine

parseSubtitle :: Parser [Text]
parseSubtitle = many parseSubtitleLine <* endOfLine

parseEntry :: Parser Entry
parseEntry = Entry <$> parseIndex <*> parseFrame <*> parseSubtitle

-- x :: MonadResource m => FilePath -> Producer m ByteString
loadFile :: FilePath -> IO ()
loadFile filename = do
  _ <- runConduitRes (sourceFileBS filename .| Conduit.foldC)
  putStrLn "Hello"
  return ()
