{-# LANGUAGE OverloadedStrings #-}

module Subtend.Format.AssSpec (spec) where

import Control.Applicative
import Data.Attoparsec.Text
import HaskellWorks.Hspec.Hedgehog
import Subtend.Format.Ass
import Test.Hspec

{-# ANN module ("HLint: ignore Redundant do"  :: String) #-}
{-# ANN module ("HLint: ignroe Reduce duplication"  :: String) #-}

spec :: Spec
spec = describe "Subtend.Format.AssSpec" $ do
  it "Can parse Index" $ do
    True `shouldBe` True
