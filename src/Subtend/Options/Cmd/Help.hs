{-# LANGUAGE DeriveAnyClass   #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell  #-}

module Subtend.Options.Cmd.Help
  ( CmdHelp(..)
  , parserCmdHelp
  ) where

import Control.Lens
import Options.Applicative

data CmdHelp = CmdHelp deriving (Show, Eq)

makeLenses ''CmdHelp

parserCmdHelp :: Parser CmdHelp
parserCmdHelp = pure CmdHelp
