module Events.Y2026.Quest11Spec (spec) where

import Events.Y2026.Quests
import SpecHelper

spec :: Spec
spec = describe "Quest 11" $ do
  it "Sample" $ do
    s <- quest11 "sample"
    s `shouldBe` (0, 0, 0)

  it "Actual" $ do
    a <- quest11 "actual"
    a `shouldBe` (0, 0, 0)
