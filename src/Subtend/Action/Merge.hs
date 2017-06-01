{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}

module Subtend.Action.Merge where

import Control.Lens
import Control.Monad
import Data.Attoparsec.Text
import Data.List
import Data.Semigroup
import Data.Text.Encoding
import Subtend.Options.Cmd.Merge
import Subtend.Ast.Srt
import Subtend.Format.Srt
import Text.PrettyPrint.ANSI.Leijen (putDoc, pretty)
import Subtend.Print.Srt

import qualified Subtend.Ast.Srt as SRT
import qualified Data.ByteString as BS

import Data.Text.Lazy.IO as LTIO
import Prelude           as P
import System.IO         as IO

actionMerge :: CmdMerge -> IO ()
actionMerge cmd = do
  IO.hPutStrLn stderr $ "Merging these files: " <> show (cmd ^. files)
  sourceData <- BS.intercalate "\n\n" <$> forM (cmd ^. files) BS.readFile
  let !parseResult = parseOnly parseEntries (decodeUtf8 sourceData)
  case parseResult of
    Right entries -> putDoc (pretty (Document (sortOn frame entries)))
    Left e        -> IO.hPutStrLn stderr $ "Error: " <> show e
  return ()
