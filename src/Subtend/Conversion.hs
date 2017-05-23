{-# LANGUAGE MultiParamTypeClasses #-}

module Subtend.Conversion where

import Data.Map as M
import Data.Maybe
import Subtend.Ast.Ass    as Ass
import Subtend.Ast.Srt    as Srt
import Subtend.Data.List
import Subtend.Data.ToMap

buildSrt :: [Map String String] -> Either String Srt.Document
buildSrt = undefined

-- groupBy :: Eq k => (a -> k) -> (a -> v) -> [a] -> Map k [v]
-- groupBy fk fv (a:as) = unionWith (++)
-- groupBy _ _ []       = empty

convert :: Ass.Document -> Either String Srt.Document
convert (Ass.Document sections) = case M.lookup "Events" (toMap sections) of
  Just entries -> case toMap entries of
    entryMap -> case M.lookup "Format" entryMap >>= listToMaybe of
      Just (Values format) -> undefined
      Nothing -> Left "'Format' entry missing"
  Nothing -> Left "[Events] section missing"
  -- case find (\s -> Ass.name s == "Events") sections of
  --   Just section -> case Ass.entries section of
  --     entries -> let x = toMap Ass.key Ass.values entries in undefined
  --     []      -> Left "No header"
  --   Nothing      -> Left "Section missing"
