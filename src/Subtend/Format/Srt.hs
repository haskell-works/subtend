{-# LANGUAGE RankNTypes #-}

module Subtend.Format.Srt where

import Conduit
import Data.Conduit
import System.IO

import qualified Data.Semigroup
import qualified Data.ByteString as BS
import qualified Data.Conduit    as C

-- x :: MonadResource m => FilePath -> Producer m ByteString
loadFile :: FilePath -> IO ()
loadFile filename = do
  _ <- runConduitRes (sourceFileBS filename .| Conduit.foldC)
  putStrLn "Hello"
  return ()
