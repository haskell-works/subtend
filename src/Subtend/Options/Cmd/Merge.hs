{-# LANGUAGE DeriveAnyClass   #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell  #-}

module Subtend.Options.Cmd.Merge
  ( CmdMerge(..)
  , files
  , parserCmdMerge
  ) where

import Control.Applicative
import Control.Lens
import Data.Semigroup
import Options.Applicative hiding (option)

newtype CmdMerge = CmdMerge
  { _files :: [String]
  } deriving (Show, Eq)

makeLenses ''CmdMerge

parserCmdMerge :: Parser CmdMerge
parserCmdMerge = CmdMerge <$> many (
  strOption
    (  long "file"
    <> metavar "FILE"
    <> help "Subtitle file" ))
