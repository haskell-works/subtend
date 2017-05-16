{-# LANGUAGE DeriveAnyClass   #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell  #-}

module Subtend.Options.Cmd where

import Control.Lens
import Data.Monoid
import Options.Applicative
import Subtend.Options.Cmd.Help  as C
import Subtend.Options.Cmd.Merge as C

data Cmd
  = CmdOfCmdHelp        { _cmdHelp        :: CmdHelp        }
  | CmdOfCmdMerge       { _cmdMerge       :: CmdMerge       }
  deriving (Show, Eq)

makeLenses ''Cmd

cmds :: Parser Cmd
cmds = subparser
  (   command "help"        (info (CmdOfCmdHelp       <$> parserCmdHelp      ) $ progDesc "Help"                        )
  <>  command "merge"       (info (CmdOfCmdMerge      <$> parserCmdMerge     ) $ progDesc "Merge"                       )
  )
