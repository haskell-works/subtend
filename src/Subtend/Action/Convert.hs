{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}

module Subtend.Action.Convert where

import Control.Lens
import Data.Semigroup
import Data.Text.Encoding
import Subtend.Format.Ass
import Subtend.Options.Cmd.Convert

import Control.Applicative
import Data.Attoparsec.Text

import qualified Data.ByteString   as BS
import qualified Data.Text.Lazy.IO as LTIO

actionConvert :: CmdConvert -> IO ()
actionConvert cmd = do
  Prelude.putStrLn $ "Converting " <> show (cmd ^. source) <> " to " <> show (cmd ^. target) <> " as " <> show (cmd ^. format)
  sourceData <- BS.readFile (cmd ^. source)
  Prelude.putStrLn $ "source data: " <> show (decodeUtf8 sourceData)
  let !parseResult = parseOnly parseDocument (decodeUtf8 sourceData)
  Prelude.putStrLn $ "parse result: " <> show parseResult
  BS.writeFile (cmd ^. target) sourceData
