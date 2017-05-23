{-# LANGUAGE TypeFamilies #-}

module Subtend.Data.ToMap where

import Data.Map

class ToMap a where
  type MapKey a
  type MapValue a
  toMap :: a -> Map (MapKey a) (MapValue a)
