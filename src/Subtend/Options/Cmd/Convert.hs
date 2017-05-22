{-# LANGUAGE DeriveAnyClass   #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell  #-}

module Subtend.Options.Cmd.Convert
  ( CmdConvert(..)
  , format
  , parserCmdConvert
  , source
  , target
  ) where

import Control.Applicative
import Control.Lens
import Data.Semigroup
import Options.Applicative hiding (option)

data CmdConvert = CmdConvert
  { _source :: String
  , _target :: String
  , _format :: String
  } deriving (Show, Eq)

makeLenses ''CmdConvert

parserCmdConvert :: Parser CmdConvert
parserCmdConvert = CmdConvert
  <$> strOption
      (  long "source"
      <> metavar "SOURCE_FILE"
      <> help "Source file" )
  <*> strOption
      (  long "target"
      <> metavar "TARGET_FILE"
      <> help "Target file" )
  <*> strOption
      (  long "format"
      <> metavar "TARGET_FORMAT"
      <> help "Target format" )
