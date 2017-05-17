{-# LANGUAGE OverloadedStrings #-}

module Subtend.Format.SrtSpec (spec) where

import Control.Applicative
import Data.Attoparsec.Text
import HaskellWorks.Hspec.Hedgehog
import Subtend.Format.Srt
import Test.Hspec

{-# ANN module ("HLint: ignore Redundant do"  :: String) #-}
{-# ANN module ("HLint: ignroe Reduce duplication"  :: String) #-}

spec :: Spec
spec = describe "Subtend.Format.SrtSpec" $ do
  it "Can parse Index" $ do
    let result = parseOnly parseIndex "1\n"
    let expected = Right 1
    result `shouldBe` expected
  it "Can parse Time" $ do
    let result = parseOnly parseTime "01:02:03,730"
    let expected = Right (Time 1 2 3 730)
    result `shouldBe` expected
  it "Can parse Frame" $ do
    let result = parseOnly parseFrame
          "00:00:03,730 --> 00:00:08,700"
    let expected = Right Frame
          { start = Time 0 0 3 730
          , stop  = Time 0 0 8 700
          }
    result `shouldBe` expected
  it "Can parse Entry" $ do
    let result = parseOnly (parseEntry <* many anyChar)
          "1\n\
          \00:00:03,730 --> 00:00:08,700\n\
          \(半沢)この産業中央銀行で\n\
          \働くことは 私の夢でした\n\
          \\n\
          \"
    let expected = Right Entry
          { index    = 1
          , frame    = Frame
            { start = Time 0 0 3 730
            , stop  = Time 0 0 8 700
            }
          , subtitle =
            [ "(半沢)この産業中央銀行で"
            , "働くことは 私の夢でした"
            ]
          }
    result `shouldBe` expected
