{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Lens
import Data.Monoid
import Data.Version
import Development.GitRev
import Options.Applicative
import Paths_subtend          (version)
import Subtend.Action.Convert
import Subtend.Action.Help
import Subtend.Action.Merge
import Subtend.Options
import Subtend.Options.Cmd

import qualified Options.Applicative as O
import qualified Subtend.Options     as O

main :: IO ()
main = do
  options <- O.execParser (O.optionsParser version $(gitHash))
  case options ^. goptCmd of
    CmdOfCmdHelp        cmd -> actionHelp       cmd
    CmdOfCmdConvert     cmd -> actionConvert    cmd
    CmdOfCmdMerge       cmd -> actionMerge      cmd
