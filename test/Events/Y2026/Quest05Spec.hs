module Events.Y2026.Quest05Spec (spec) where

import Events.Y2026.Quests
import SpecHelper

spec :: Spec
spec = describe "Quest 5" $ do
  it "Sample" $ do
    s <- quest05 "sample"
    s `shouldBe` (0, 0, 0)

  it "Actual" $ do
    a <- quest05 "actual"
    a `shouldBe` (0, 0, 0)
