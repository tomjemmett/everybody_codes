module Stories.S3.Quest03Spec (spec) where

import SpecHelper
import Stories.S3.Quests

spec :: Spec
spec = describe "Story 3, Quest 3" $ do
  it "Sample" $ do
    s <- quest03 "sample"
    s `shouldBe` (43, 50, 60)

  it "Actual" $ do
    a <- quest03 "actual"
    a `shouldBe` (6377, 319598, 441414)
