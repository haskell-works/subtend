{-# LANGUAGE MultiParamTypeClasses #-}

module Subtend.Conversion where

import Data.Map
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
convert (Ass.Document sections) = let x = toMap sections in undefined
  -- case find (\s -> Ass.name s == "Events") sections of
  --   Just section -> case Ass.entries section of
  --     entries -> let x = toMap Ass.key Ass.values entries in undefined
  --     []      -> Left "No header"
  --   Nothing      -> Left "Section missing"
