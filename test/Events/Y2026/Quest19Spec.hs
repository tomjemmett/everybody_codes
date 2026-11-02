module Events.Y2026.Quest19Spec (spec) where

import Events.Y2026.Quests
import SpecHelper

spec :: Spec
spec = describe "Quest 19" $ do
  it "Sample" $ do
    s <- quest19 "sample"
    s `shouldBe` (0, 0, 0)

  it "Actual" $ do
    a <- quest19 "actual"
    a `shouldBe` (0, 0, 0)
