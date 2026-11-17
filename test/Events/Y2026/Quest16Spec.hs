module Events.Y2026.Quest16Spec (spec) where

import Events.Y2026.Quests
import SpecHelper

spec :: Spec
spec = describe "Quest 16" $ do
  it "Sample" $ do
    s <- quest16 "sample"
    s `shouldBe` (26, 1053, 135154)

  it "Actual" $ do
    a <- quest16 "actual"
    a `shouldBe` (58, 24745673, 24783682651354)