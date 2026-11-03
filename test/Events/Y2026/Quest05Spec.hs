module Events.Y2026.Quest05Spec (spec) where

import Events.Y2026.Quests
import SpecHelper

spec :: Spec
spec = describe "Quest 5" $ do
  it "Sample" $ do
    s <- quest05 "sample"
    s `shouldBe` (137, 69, 14)

  it "Actual" $ do
    a <- quest05 "actual"
    a `shouldBe` (1192, 1084, 9749)
