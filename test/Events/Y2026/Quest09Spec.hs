module Events.Y2026.Quest09Spec (spec) where

import Events.Y2026.Quests
import SpecHelper

spec :: Spec
spec = describe "Quest 9" $ do
  it "Sample" $ do
    s <- quest09 "sample"
    s `shouldBe` (0, 0, 0)


  it "Actual" $ do
    a <- quest09 "actual"
    a `shouldBe` (0, 0, 0)
