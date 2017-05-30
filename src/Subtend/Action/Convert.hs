{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}

module Subtend.Action.Convert where

import Control.Applicative
import Control.Lens
import Data.Attoparsec.Text
import Data.Semigroup
import Data.Text.Encoding
import Subtend.Conversion
import Subtend.Format.Ass
import Subtend.Options.Cmd.Convert
import Subtend.Print.Srt
import System.IO                    (hClose, openBinaryFile, IOMode(..))
import Text.PrettyPrint.ANSI.Leijen (hPutDoc, pretty)

import qualified Data.ByteString   as BS
import qualified Data.Text.Lazy.IO as LTIO

actionConvert :: CmdConvert -> IO ()
actionConvert cmd = do
  Prelude.putStrLn
    $  "Converting "  <> show (cmd ^. source)
    <> " to "         <> show (cmd ^. target)
    <> " as "         <> show (cmd ^. format)
  sourceData <- BS.readFile (cmd ^. source)
  let !parseResult = parseOnly parseDocument (decodeUtf8 sourceData)
  case parseResult of
    Right doc -> case convert doc of
      Right srtDoc -> do
        hOut <- openBinaryFile (cmd ^. target) WriteMode
        hPutDoc hOut (pretty srtDoc)
        hClose hOut
      Left _       -> Prelude.putStrLn "Could not convert"
    Left _    -> Prelude.putStrLn "moo"
