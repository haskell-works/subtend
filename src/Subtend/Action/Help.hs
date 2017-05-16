{-# LANGUAGE OverloadedStrings #-}

module Subtend.Action.Help where

import Data.Text.Lazy.IO        as LTIO
import Subtend.Options.Cmd.Help

actionHelp :: CmdHelp -> IO ()
actionHelp _ = LTIO.putStrLn "No help yet"
