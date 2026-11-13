module Events.Y2026.Quest14Spec (spec) where

import Events.Y2026.Quests
import SpecHelper

spec :: Spec
spec = describe "Quest 14" $ do
  it "Sample" $ do
    s <- quest14 "sample"
    s `shouldBe` (67, 63, 21)

  it "Actual" $ do
    a <- quest14 "actual"
    a `shouldBe` (268, 391, 1716)
