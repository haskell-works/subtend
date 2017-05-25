{-# LANGUAGE MultiParamTypeClasses #-}

module Subtend.Conversion where

import Data.Map as M
import Data.Maybe
import Subtend.Ast.Ass    as Ass
import Subtend.Ast.Srt    as Srt
import Subtend.Data.List
import Subtend.Data.ToMap

import qualified Data.Text as T

buildSrt :: [Map String String] -> Either String Srt.Document
buildSrt = undefined

asTime :: String -> Maybe Srt.Time
asTime = undefined

formatAndDialogueToSrtEntries :: Values -> Values -> Maybe Srt.Entry
formatAndDialogueToSrtEntries (Values ks) (Values vs) = Srt.Entry 0 <$> frame <*> text
  where kvs   = M.fromList (ks `zip` vs)
        frame = Srt.Frame
                <$> (M.lookup "Start" kvs >>= asTime)
                <*> (M.lookup "Stop"  kvs >>= asTime)
        text  = (:[]) . T.pack <$> M.lookup "Text" kvs

convert :: Ass.Document -> Either String Srt.Document
convert (Ass.Document sections) = case M.lookup "Events" (toMap sections) of
  Just entries -> case toMap entries of
    entryMap -> case M.lookup "Format" entryMap >>= listToMaybe of
      Just format -> case M.lookup "Dialogue" entryMap of
        Just dialogues -> Right (Srt.Document (catMaybes (formatAndDialogueToSrtEntries format <$> dialogues)))
      Nothing -> Left "'Format' entry missing"
  Nothing -> Left "[Events] section missing"
