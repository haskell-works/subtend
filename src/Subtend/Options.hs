{-# LANGUAGE TemplateHaskell #-}

module Subtend.Options where

import           Control.Lens
import           Data.Monoid
import           Data.Version
import           Options.Applicative

import           HaskellWorks.Ci.Options.Cmd

data HelpOptions = HelpOptions deriving (Show, Eq)

newtype GlobalOptions = GlobalOptions
  { _goptCmd :: Cmd
  }
  deriving (Show, Eq)

makeLenses ''GlobalOptions

parserGlobalOptions :: Parser GlobalOptions
parserGlobalOptions = GlobalOptions <$> cmds

optionsParser :: Version -> String -> ParserInfo GlobalOptions
optionsParser version gitHash = info (helper <*> versionOption <*> parserGlobalOptions)
  (  fullDesc
  <> progDesc "CLI tool for Continuous Integration"
  <> header "CI tool"
  )
  where versionOption = infoOption
          (concat [showVersion version, " ", gitHash])
          (long "version" <> help "Show version")
