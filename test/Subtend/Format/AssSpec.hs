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
  it "Can parse parseSectionName" $ do
    let result = parseOnly parseSectionName "[Section Name]"
    let expected = Right "Section Name"
    result `shouldBe` expected
  it "Can parse comment" $ do
    let result = parseOnly parseComment "; Script generated by Aegisub 3.0.1"
    let expected = Right ()
    result `shouldBe` expected
  describe "Can parse identifier" $ do
    it "no spaces" $ do
      let result = parseOnly parseIdentifier "ScaledBorderAndShadow"
      let expected = Right "ScaledBorderAndShadow"
      result `shouldBe` expected
    it "with spaces" $ do
      let result = parseOnly parseIdentifier "Last Style Storage"
      let expected = Right "Last Style Storage"
      result `shouldBe` expected
  describe "Can parse parseEntry" $ do
    it "without trailing newline" $ do
      let result = parseOnly (parseEntry <* endOfInput) "Title: Default Aegisub file"
      let expected = Right (Entry "Title" ["Default Aegisub file"])
      result `shouldBe` expected
    it "with trailing newline" $ do
      let result = parseOnly (parseEntry <* endOfInput) "Title: Default Aegisub file\n"
      let expected = Right (Entry "Title" ["Default Aegisub file"])
      result `shouldBe` expected
  it "Can parse parseEntries" $ do
    let result = parseOnly (many parseEntry) "\
          \Title: Default Aegisub file\n\
          \ScriptType: v4.00+\n\
          \WrapStyle: 0\n\
          \ScaledBorderAndShadow: yes\n\
          \Collisions: Normal\n\
          \Scroll Position: 785\n\
          \Active Line: 791\n\
          \Video Zoom Percent: 1\n\
          \PlayResX: 848\n\
          \PlayResY: 480\n\
          \Last Style Storage: Default\n\
          \YCbCr Matrix: TV.601\n\
          \Video Zoom: 6\n\
          \"
    let expected = Right
          [ Entry "Title"                 ["Default Aegisub file"]
          , Entry "ScriptType"            ["v4.00+"]
          , Entry "WrapStyle"             ["0"]
          , Entry "ScaledBorderAndShadow" ["yes"]
          , Entry "Collisions"            ["Normal"]
          , Entry "Scroll Position"       ["785"]
          , Entry "Active Line"           ["791"]
          , Entry "Video Zoom Percent"    ["1"]
          , Entry "PlayResX"              ["848"]
          , Entry "PlayResY"              ["480"]
          , Entry "Last Style Storage"    ["Default"]
          , Entry "YCbCr Matrix"          ["TV.601"]
          , Entry "Video Zoom"            ["6"]
          ]
    result `shouldBe` expected
  it "Can parse document" $ do
    let result = parseOnly parseDocument "\
          \[Script Info]\n\
          \Title: Default Aegisub file\n\
          \ScriptType: v4.00+\n\
          \WrapStyle: 0\n\
          \ScaledBorderAndShadow: yes\n\
          \Collisions: Normal\n\
          \Scroll Position: 785\n\
          \Active Line: 791\n\
          \Video Zoom Percent: 1\n\
          \PlayResX: 848\n\
          \PlayResY: 480\n\
          \Last Style Storage: Default\n\
          \YCbCr Matrix: TV.601\n\
          \Video Zoom: 6\n\
          \\n\
          \[V4+ Styles]\n\
          \Format: Name, Fontname, Fontsize, PrimaryColour, SecondaryColour, OutlineColour, BackColour, Bold, Italic, Underline, StrikeOut, ScaleX, ScaleY, Spacing, Angle, BorderStyle, Outline, Shadow, Alignment, MarginL, MarginR, MarginV, Encoding\n\
          \Style: Default,Arial,36,&H00FFFFFF,&H000000FF,&H00000000,&H00000000,0,0,0,0,100,100,0,0,1,2,2,2,10,10,10,1\n\
          \Style: Caption,Arial,20,&H0000E7FF,&H00FFFFFF,&H00000000,&H00000000,0,0,0,0,100,100,0,0,1,2,2,2,10,10,10,1\n\
          \Style: Caption 2,Arial,30,&H00000BEF,&H00000000,&H00000000,&H00000000,0,0,0,0,100,100,0,0,1,2,2,2,10,10,10,1\n\
          \Style: White,Tahoma,30,&H00FFFFFF,&H0000E4FF,&H00000000,&H00000000,0,-1,0,0,100,100,0,0,1,2,1,2,10,10,10,1\n\
          \\n\
          \[Events]\n\
          \Format: Layer, Start, End, Style, Name, MarginL, MarginR, MarginV, Effect, Text\n\
          \Dialogue: 0,0:00:03.23,0:00:07.86,Default,,0,0,0,,It's been my dream to\\Nwork here at Sangyo Chuo Bank.\n\
          \Dialogue: 0,0:00:09.02,0:00:11.84,Default,,0,0,0,,However, we're not\\Nthe only bank around, are we?\n\
          \"
    let expected = Right $ Document
          [ Section
            { name = "Script Info"
            , entry =
              [ Entry "Title"                 ["Default Aegisub file"]
              , Entry "ScriptType"            ["v4.00+"]
              , Entry "WrapStyle"             ["0"]
              , Entry "ScaledBorderAndShadow" ["yes"]
              , Entry "Collisions"            ["Normal"]
              , Entry "Scroll Position"       ["785"]
              , Entry "Active Line"           ["791"]
              , Entry "Video Zoom Percent"    ["1"]
              , Entry "PlayResX"              ["848"]
              , Entry "PlayResY"              ["480"]
              , Entry "Last Style Storage"    ["Default"]
              , Entry "YCbCr Matrix"          ["TV.601"]
              , Entry "Video Zoom"            ["6"]
              ]
            }
          , Section
            { name = "V4+ Styles"
            , entry =
              [ Entry "Format"                ["Name"     , "Fontname", "Fontsize", "PrimaryColour", "SecondaryColour", "OutlineColour","BackColour","Bold","Italic","Underline","StrikeOut","ScaleX","ScaleY","Spacing","Angle","BorderStyle","Outline","Shadow","Alignment","MarginL","MarginR","MarginV","Encoding"]
              , Entry "Style"                 ["Default"  , "Arial"   , "36"      , "&H00FFFFFF"   , "&H000000FF"     , "&H00000000"   ,"&H00000000","0"   ,"0"     ,"0"        ,"0"        ,"100"   ,"100"   ,"0"      ,"0"    ,"1"          ,"2"      ,"2"     ,"2"        ,"10"     ,"10"     ,"10"     ,"1"       ]
              , Entry "Style"                 ["Caption"  , "Arial"   , "20"      , "&H0000E7FF"   , "&H00FFFFFF"     , "&H00000000"   ,"&H00000000","0"   ,"0"     ,"0"        ,"0"        ,"100"   ,"100"   ,"0"      ,"0"    ,"1"          ,"2"      ,"2"     ,"2"        ,"10"     ,"10"     ,"10"     ,"1"       ]
              , Entry "Style"                 ["Caption 2", "Arial"   , "30"      , "&H00000BEF"   , "&H00000000"     , "&H00000000"   ,"&H00000000","0"   ,"0"     ,"0"        ,"0"        ,"100"   ,"100"   ,"0"      ,"0"    ,"1"          ,"2"      ,"2"     ,"2"        ,"10"     ,"10"     ,"10"     ,"1"       ]
              , Entry "Style"                 ["White"    , "Tahoma"  , "30"      , "&H00FFFFFF"   , "&H0000E4FF"     , "&H00000000"   ,"&H00000000","0"   ,"-1"    ,"0"        ,"0"        ,"100"   ,"100"   ,"0"      ,"0"    ,"1"          ,"2"      ,"1"     ,"2"        ,"10"     ,"10"     ,"10"     ,"1"       ]
              ]
            }
          , Section
            { name = "Events"
            , entry =
              [ Entry "Format"                ["Layer", "Start"     ,"End"       ,"Style"  ,"Name","MarginL","MarginR","MarginV","Effect","Text"                                                  ]
              , Entry "Dialogue"              ["0"    , "0:00:03.23","0:00:07.86","Default",""    ,"0"      ,"0"      ,"0"      ,""      ,"It's been my dream to\\Nwork here at Sangyo Chuo Bank."]
              , Entry "Dialogue"              ["0"    , "0:00:09.02","0:00:11.84","Default",""    ,"0"      ,"0"      ,"0"      ,""      ,"However","we're not\\Nthe only bank around","are we?"  ]
              ]
            }
          ]
    result `shouldBe` expected
