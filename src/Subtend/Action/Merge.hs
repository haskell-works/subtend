{-# LANGUAGE OverloadedStrings #-}

module Subtend.Action.Merge where

import Control.Lens
import Data.Semigroup
import Data.Text.Lazy.IO         as LTIO
import Subtend.Options.Cmd.Merge

actionMerge :: CmdMerge -> IO ()
actionMerge cmd = do
  LTIO.putStrLn "No merge yet"
  Prelude.putStrLn $ "Should merge these files: " <> show (cmd ^. files)
