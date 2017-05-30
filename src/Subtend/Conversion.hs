{-# LANGUAGE MultiParamTypeClasses #-}

module Subtend.Conversion where

import Data.Attoparsec.Text
import Data.Maybe
import Subtend.Data.List
import Subtend.Data.ToMap
import Subtend.Duration
import Data.Semigroup
import Data.Text.Lazy.Builder

import qualified Data.Map           as M
import qualified Data.Text          as T
import qualified Subtend.Ast.Ass    as ASS
import qualified Subtend.Ast.Srt    as SRT
import qualified Subtend.Format.Ass as ASS

asTime :: String -> Maybe SRT.Time
asTime text = either (const Nothing) (Just . viaDuration) (parseOnly ASS.parseTime (T.pack text))

zipSquish :: (b -> b -> b) -> [a] -> [b] -> [(a, b)]
zipSquish f [a]    (b:c:bs) = zipSquish f [a] (f b c:bs)
zipSquish f (a:as) (b:bs)   = (a, b):zipSquish f as bs
zipSquish _ _      _        = []

formatAndDialogueToSrtEntries :: ASS.Values -> ASS.Values -> Maybe SRT.Entry
formatAndDialogueToSrtEntries (ASS.Values ks) (ASS.Values vs) =
  SRT.Entry 0 <$> frame <*> text
  where kvs   = M.fromList (zipSquish (\a b -> a <> "," <> b) ks vs)
        frame = SRT.Frame
                <$> (M.lookup "Start" kvs >>= asTime)
                <*> (M.lookup "End"   kvs >>= asTime)
        text  = (:[]) . T.pack <$> M.lookup "Text" kvs

convert :: ASS.Document -> Either String SRT.Document
convert (ASS.Document sections) = case M.lookup "Events" (toMap sections) of
  Just entries -> case toMap entries of
    entryMap -> case M.lookup "Format" entryMap >>= listToMaybe of
      Just format -> case M.lookup "Dialogue" entryMap of
        Just dialogues -> Right (SRT.Document (catMaybes (formatAndDialogueToSrtEntries format <$> dialogues)))
      Nothing -> Left "'Format' entry missing"
  Nothing -> Left "[Events] section missing"
