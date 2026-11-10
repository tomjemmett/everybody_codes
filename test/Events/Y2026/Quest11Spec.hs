module Events.Y2026.Quest11Spec (spec) where

import Events.Y2026.Quests
import SpecHelper

spec :: Spec
spec = describe "Quest 11" $ do
  it "Sample" $ do
    s <- quest11 "sample"
    s `shouldBe` (60, 6, 9634)

  it "Actual" $ do
    a <- quest11 "actual"
    a `shouldBe` (1580, 2274, 28052)
