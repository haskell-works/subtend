{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns    #-}

module Subtend.Print.Srt where

import Data.Text                    (Text, unpack)
import Subtend.Ast.Srt
import Subtend.Normalise
import Text.PrettyPrint.ANSI.Leijen
import Debug.Trace

newtype Subtitle = Subtitle [Text] deriving (Eq, Show)

instance Pretty Document where
  pretty (Document es) = go (normalise es)
    where go (e:es) =   pretty e
                    <>  hardline
                    <>  go es
          go _ = empty

instance Pretty Entry where
  pretty Entry {index, frame, subtitle}
    =   pretty index
    <>  hardline
    <>  pretty frame
    <>  hardline
    <>  pretty (Subtitle subtitle)
    <>  hardline

instance Pretty Subtitle where
  pretty (Subtitle [e])
    =   text (unpack e)
  pretty (Subtitle (e:es))
    =   text (unpack e)
    <>  hardline
    <>  pretty (Subtitle es)
  pretty _ = empty

instance Pretty Frame where
  pretty Frame {start, stop}
    =   pretty start
    <+> text "-->"
    <+> pretty stop

data Digits a = Digits Int a deriving (Eq, Show)

instance (Integral a, Pretty a) => Pretty (Digits a) where
  pretty (Digits n a) | n > 0 = let (d, m) = a `divMod` 10 in pretty (Digits (n - 1) d) <> pretty m
  pretty _                    = empty

instance Pretty Time where
  pretty Time {hour, minutes, seconds, millis}
      =   pretty (Digits 2 hour)
      <>  text ":"
      <>  pretty (Digits 2 minutes)
      <>  text ":"
      <>  pretty (Digits 2 seconds)
      <>  text ","
      <>  pretty (Digits 3 millis)
