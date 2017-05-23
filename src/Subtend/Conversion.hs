{-# LANGUAGE MultiParamTypeClasses #-}

module Subtend.Conversion where

-- import Data.Map
-- import Subtend.Ast.Ass as Ass
-- import Subtend.Ast.Srt as Srt

-- find :: Eq a => (a -> Bool) -> [a] -> Maybe a
-- find p (a:as) | p a = Just a
-- find p (_:as) = find p as
-- find _ []     = Nothing

-- buildSrt :: [Map String String] -> Either String Srt.Document
-- buildSrt = undefined

-- groupBy :: Eq k => (a -> k) -> (a -> v) -> [a] -> Map k [v]
-- groupBy fk fv (a:as) = unionWith (++)
-- groupBy _ _ []       = empty

-- convert :: Ass.Document -> Either String Srt.Document
-- convert (Ass.Document sections) =
--   case find (\s -> Ass.name s == "Events") sections of
--     Just section -> case Ass.entries section of
--       entries -> groupBy Ass.key Ass.values entries
--       []      -> Left "No header"
--     Nothing      -> Left "Section missing"
