module Events.Y2026.Quest20Spec (spec) where

import Events.Y2026.Quests
import SpecHelper

spec :: Spec
spec = describe "Quest 20" $ do
  it "Sample" $ do
    s <- quest20 "sample"
    s `shouldBe` (75, 101, 125)

  it "Actual" $ do
    a <- quest20 "actual"
    a `shouldBe` (398, 45181, 47598)